use core::hash::{BuildHasher, Hash};
use std::ops::DerefMut;

use bevy::{
    asset::io::AssetSourceBuilder,
    platform_support::{
        collections::HashMap,
        hash::{FixedHasher, NoOpHash},
    },
    prelude::*,
};
use bevy_bsn_ast::syn::{self, spanned::Spanned};

use crate::{DynamicPatch, EntityPatch, Patch, Scene};

mod rs_file;
use rs_file::*;
mod hot_patch;
pub use hot_patch::{BsnInvocation, HotPatch};
mod visit;
use visit::*;

/// Extension trait for [`App`] to allow registering hot-reload sources.
pub trait HotReloadApp {
    /// Registers a source directory for .rs-files to be hot-reloaded.
    fn register_hot_reload_source(&mut self, dir: &'static str) -> &mut Self;
}

impl HotReloadApp for App {
    fn register_hot_reload_source(&mut self, dir: &'static str) -> &mut Self {
        self.register_asset_source(dir, AssetSourceBuilder::platform_default(dir, None))
    }
}

/// Adds hot-reload support for [`crate::bsn!`] macro invocations.
pub struct HotReloadPlugin;

impl Plugin for HotReloadPlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<RsFile>();
        app.init_asset_loader::<RsFileLoader>();
        app.init_resource::<HotReloadState>();
        app.add_systems(PreStartup, hot_reload_setup);
        app.add_systems(Update, (hot_reload_added, hot_reload_modified).chain());
    }
}

/// Resource holding hot-reload state for `bsn!`-invocations.
#[derive(Resource, Default)]
pub struct HotReloadState {
    /// Handles to source files containing bsn macro invocations.
    handles: HashMap<AssetId<RsFile>, Handle<RsFile>>,
    /// Maps .rs-files to their macro invocations.
    invocation_ids: HashMap<AssetId<RsFile>, Vec<InvocationId>>,
    /// Invocation info for each hot reloadable bsn!-macro.
    invocations: HashMap<InvocationId, BsnInvocation, NoOpHash>,
}

impl HotReloadState {
    /// Adds a [`HotPatch`] if there are any hot reloaded changes.
    ///
    /// See: [`BsnInvocation::init_hot_patch`]
    #[inline]
    pub fn init_hot_patch<I, P, C>(
        &mut self,
        entity_patch: &mut EntityPatch<I, P, C>,
        world: &World,
    ) where
        I: Scene,
        P: Patch + DynamicPatch,
        C: Scene,
    {
        if let Some(invocation_id) = entity_patch.invocation_id {
            if let Some(invocation) = self.invocations.get_mut(&invocation_id) {
                invocation.init_hot_patch(entity_patch, world);
            }
        }
    }
}

/// Utility function for hashing a value.
#[inline]
fn hash<T: Hash>(t: T) -> u64 {
    FixedHasher.hash_one(&t)
}

/// Identifies a specific `bsn!`-macro invocation in the _original_ source files.
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub struct InvocationId(u64);

impl InvocationId {
    /// Creates a new [`InvocationId`] by hashing the given `path`, `line`, and `column`.
    pub fn new(path: &str, line: u32, column: u32) -> Self {
        Self(hash((path, line, column)))
    }
}

/// Initializes the hot-reload state by loading the source files as assets.
fn hot_reload_setup(mut state: ResMut<HotReloadState>, asset_server: Res<AssetServer>) {
    // TODO: Actually use the non-hardcoded, configured sources
    let handle = asset_server.load::<RsFile>("examples://bsn_macro_hot_reload.rs");
    state.handles.insert(handle.id(), handle);
}

/// Initializes the state for loaded .rs-files.
fn hot_reload_added(
    mut state: ResMut<HotReloadState>,
    assets: Res<Assets<RsFile>>,
    mut event_reader: EventReader<AssetEvent<RsFile>>,
) {
    for event in event_reader.read() {
        if let AssetEvent::Added { id } = event {
            let file = assets.get(*id).unwrap();

            // Parse file and visit bsn! invocations
            let ast = match syn::parse_file(&file.content) {
                Ok(ast) => ast,
                Err(e) => {
                    error!("Failed to parse source file {}: {}", file.path, e);
                    continue;
                }
            };
            let macro_invocations = visit_and_collect_macros(&ast);

            // Don't keep handles for files without bsn! invocations
            if macro_invocations.is_empty() {
                state.handles.remove(id);
                continue;
            }

            // Store the invocation ids for this file
            let invocation_ids: Vec<_> = macro_invocations
                .iter()
                .map(|invocation| {
                    let span = invocation.span();
                    InvocationId::new(
                        &file.path,
                        span.start().line as u32,
                        (span.start().column + 1) as u32,
                    )
                })
                .collect();

            // Store the original reflected scenes for this file
            for (invocation, invocation_id) in macro_invocations.iter().zip(invocation_ids.iter()) {
                let bsn_invocation = match BsnInvocation::try_from_original(invocation) {
                    Ok(bsn_invocation) => bsn_invocation,
                    Err(e) => {
                        warn!("Failed to parse bsn! in {:?}: {}", file.path, e);
                        continue;
                    }
                };

                state.invocations.insert(*invocation_id, bsn_invocation);
            }

            state.invocation_ids.insert(*id, invocation_ids);
        }
    }
}

/// Performs hot-reload of macro invocations in modified .rs-files.
fn hot_reload_modified(
    mut state: ResMut<HotReloadState>,
    assets: Res<Assets<RsFile>>,
    mut event_reader: EventReader<AssetEvent<RsFile>>,
) {
    for event in event_reader.read() {
        if let AssetEvent::Modified { id } = event {
            let file = assets.get(*id).unwrap();

            // Parse file and visit bsn! invocations
            let ast = match syn::parse_file(&file.content) {
                Ok(ast) => ast,
                Err(e) => {
                    error!("Failed to parse source file {}: {}", file.path, e);
                    continue;
                }
            };
            let macro_invocations = visit_and_collect_macros(&ast);

            let HotReloadState {
                handles,
                invocation_ids,
                invocations,
            } = state.deref_mut();

            let Some(invocation_ids) = invocation_ids.get(id) else {
                continue;
            };

            // Ensure that the number of invocations has not changed because we rely on index for identification between loads.
            if macro_invocations.len() != invocation_ids.len() {
                warn!("bsn!-invocation count changed in {:?}, this file will not be hot reloaded until the next recompile.", file.path);
                handles.remove(id);
                continue;
            }

            for (invocation, invocation_id) in
                macro_invocations.into_iter().zip(invocation_ids.iter())
            {
                // Get the original reflected patch
                let Some(bsn_invocation) = invocations.get_mut(invocation_id) else {
                    warn!(
                        "No original patch found for invocation {:?} in {:?}, skipping hot reload.",
                        invocation_id, file.path
                    );
                    continue;
                };

                // Reload the changes
                if let Err(e) = bsn_invocation.reload_invocation(invocation) {
                    warn!(
                        "Failed to parse hot-reloaded bsn! in {:?}: {}",
                        file.path, e
                    );
                }
            }
        }
    }
}
