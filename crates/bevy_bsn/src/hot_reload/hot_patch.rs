use std::any::TypeId;

use bevy::{
    log::{info, warn},
    platform_support::{collections::HashSet, hash::NoOpHash},
    reflect::{PartialReflect, TypeRegistry},
    utils::TypeIdMap,
};
use bevy_bsn_ast::{syn, BsnAstEntity};

use crate::{Bsn, BsnReflector, DynamicPatch, DynamicScene};

use super::hash;

/// A scene patch parsed and reflected from a bsn!-invocation in a source file.
#[derive(Default, Debug)]
pub struct HotPatch {
    original_hash: u64,
    current_hash: u64,
    component_patches: TypeIdMap<Box<dyn PartialReflect>>,
    removed_components: HashSet<TypeId, NoOpHash>,
    // TODO: Children
}

impl Clone for HotPatch {
    fn clone(&self) -> Self {
        Self {
            original_hash: self.original_hash,
            current_hash: self.current_hash,
            component_patches: self
                .component_patches
                .iter()
                .map(|(type_id, patch)| (*type_id, patch.clone_value()))
                .collect(),
            removed_components: self.removed_components.clone(),
        }
    }
}

impl DynamicPatch for HotPatch {
    fn dynamic_patch(self, scene: &mut DynamicScene) {
        for (type_id, patch) in self.component_patches {
            scene.patch_reflected(type_id, move |props: &mut dyn PartialReflect| {
                props.apply(patch.as_ref());
            });
        }

        for type_id in self.removed_components {
            scene.remove_component(type_id);
        }
    }
}

impl HotPatch {
    /// Clones this hot patch if it has any changes.
    pub fn clone_if_changed(&self) -> Option<Self> {
        if self.has_changes() {
            Some(self.clone())
        } else {
            None
        }
    }

    /// Returns true if this hot patch has any reloaded changes from the original macro.
    pub fn has_changes(&self) -> bool {
        self.original_hash != self.current_hash
    }

    /// Tries to parse and reflect a [`HotPatch`] from an original bsn! macro invocation.
    pub fn try_from_macro(
        macro_ast: &syn::Macro,
        registry: &TypeRegistry,
    ) -> Result<Self, syn::Error> {
        let hash = hash(macro_ast);
        let bsn_ast = syn::parse2::<BsnAstEntity>(macro_ast.tokens.clone())?;
        let bsn = Bsn::from(&bsn_ast);
        Ok(Self::from_bsn(hash, &bsn, registry))
    }

    /// Creates a [`HotPatch`] from a parsed BSN ast.
    pub fn from_bsn(hash: u64, bsn: &Bsn, registry: &TypeRegistry) -> Self {
        let reflector = BsnReflector::new(bsn, registry);
        let mut component_patches =
            TypeIdMap::with_capacity_and_hasher(bsn.root.components.len(), NoOpHash);
        for bsn_component in bsn.root.components.iter() {
            // TODO: Field-level error handling to allow partial reflection
            let Ok(reflected_component) = reflector.reflect_component_patch(bsn_component) else {
                warn!("Failed to reflect component: {:?}", bsn_component);
                continue;
            };

            component_patches.insert(
                reflected_component.type_id,
                reflected_component.props.instance,
            );
        }

        Self {
            original_hash: hash,
            current_hash: hash,
            component_patches,
            removed_components: Default::default(),
        }
    }

    /// Update this hot patch with the changes from a reloaded macro invocation.
    pub fn apply_reloaded(
        &mut self,
        macro_ast: &syn::Macro,
        registry: &TypeRegistry,
    ) -> Result<(), syn::Error> {
        let new_hash = hash(macro_ast);
        if new_hash == self.current_hash {
            return Ok(());
        }
        self.current_hash = new_hash;

        let new = Self::try_from_macro(macro_ast, registry)?;
        if new_hash == self.original_hash {
            *self = new;
            return Ok(());
        }

        let mut removed_components = HashSet::with_hasher(NoOpHash);
        for type_id in self.component_patches.keys() {
            if !new.component_patches.contains_key(type_id) {
                removed_components.insert(*type_id);
            }
        }

        self.component_patches = new.component_patches;

        info!("Hot patch updated: {:?}", self);

        Ok(())
    }
}
