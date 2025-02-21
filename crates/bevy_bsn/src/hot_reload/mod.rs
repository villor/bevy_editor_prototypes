use core::hash::{BuildHasher, Hash};
use std::ops::DerefMut;

use bevy::{
    asset::io::AssetSourceBuilder,
    platform_support::{collections::HashMap, hash::FixedHasher},
    prelude::*,
};
use bevy_bsn_ast::syn::{self, spanned::Spanned};

mod rs_file;
use rs_file::*;
mod hot_patch;
pub use hot_patch::HotPatch;
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
    /// Patches for each hot reloadable bsn! invocation.
    hot_patches: HashMap<InvocationId, HotPatch>,
}

impl HotReloadState {
    /// Returns true if the given invocation has hot-reloaded changes.
    pub fn has_changes(&self, invocation_id: InvocationId) -> bool {
        self.hot_patches
            .get(&invocation_id)
            .is_some_and(HotPatch::has_changes)
    }

    /// Clones the hot patch for the given invocation id if it has hot-reloaded changes.
    pub fn clone_hot_patch_if_changed(&self, invocation_id: InvocationId) -> Option<HotPatch> {
        self.hot_patches
            .get(&invocation_id)
            .and_then(HotPatch::clone_if_changed)
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
        info_once!("path: {:?}, line: {:?}, column: {:?}", path, line, column);
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
    registry: Res<AppTypeRegistry>,
) {
    let registry = registry.read();
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
            let invocations = visit_and_collect_macros(&ast);

            // Don't keep handles for files without bsn! invocations
            if invocations.is_empty() {
                state.handles.remove(id);
                continue;
            }

            // Store the invocation ids for this file
            let invocation_ids: Vec<_> = invocations
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
            for (invocation, invocation_id) in invocations.iter().zip(invocation_ids.iter()) {
                let hot_scene = match HotPatch::try_from_macro(invocation, &registry) {
                    Ok(hot_scene) => hot_scene,
                    Err(e) => {
                        warn!("Failed to parse bsn! in {:?}: {}", file.path, e);
                        continue;
                    }
                };

                info!("invocation id: {:?}", invocation_id);

                state.hot_patches.insert(*invocation_id, hot_scene);
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
    // components: &Components,
    registry: Res<AppTypeRegistry>,
) {
    let registry = registry.read();
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
            let invocations = visit_and_collect_macros(&ast);

            let HotReloadState {
                handles,
                invocation_ids,
                hot_patches,
            } = state.deref_mut();

            let Some(invocation_ids) = invocation_ids.get(id) else {
                continue;
            };

            // Ensure that the number of invocations has not changed because we rely on index for identification between loads.
            if invocations.len() != invocation_ids.len() {
                warn!("bsn!-invocation count changed in {:?}, this file will not be hot reloaded until the next recompile.", file.path);
                handles.remove(id);
                continue;
            }

            for (invocation, invocation_id) in invocations.into_iter().zip(invocation_ids.iter()) {
                // Get the original reflected patch
                let Some(hot_patch) = hot_patches.get_mut(invocation_id) else {
                    warn!(
                        "No previous patch found for invocation {:?} in {:?}, skipping hot reload.",
                        invocation_id, file.path
                    );
                    continue;
                };

                // Apply the hot-reloaded patch
                if let Err(e) = hot_patch.apply_reloaded(invocation, &registry) {
                    warn!(
                        "Failed to parse hot-reloaded bsn! in {:?}: {}",
                        file.path, e
                    );
                }
            }
        }
    }
}
