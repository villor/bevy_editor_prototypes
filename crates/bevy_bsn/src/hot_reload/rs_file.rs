use std::path::Path;

use bevy::{
    asset::{io::Reader, Asset, AssetLoader, AsyncReadExt, LoadContext},
    reflect::TypePath,
};
use thiserror::Error;

/// A .rs-file loaded as an asset.
#[derive(Asset, TypePath, Debug)]
pub struct RsFile {
    /// Path, relative to the asset root.
    pub path: String,
    /// Contents of the source file
    pub content: String,
}

/// Asset loader for loading source files.
#[derive(Default)]
pub struct RsFileLoader;

/// Error for [`RsFileLoader`]
#[non_exhaustive]
#[derive(Debug, Error)]
pub enum RsFileLoaderError {
    /// An [IO](std::io) Error
    #[error("Could not load source file: {0}")]
    Io(#[from] std::io::Error),
}

impl AssetLoader for RsFileLoader {
    type Asset = RsFile;
    type Settings = ();
    type Error = RsFileLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut content = String::new();
        reader.read_to_string(&mut content).await?;

        let asset_path = load_context.asset_path();
        // TODO: Fix the path stuff
        // Workspace dir: cargo locate-project --message-format plain --workspace
        // Package dir: cargo locate-project --message-format plain
        let path = Path::join(
            Path::join(
                Path::new("crates/bevy_bsn"), // TODO
                Path::new(asset_path.source().as_str().unwrap_or("")),
            )
            .as_path(),
            asset_path.path(),
        )
        .to_string_lossy()
        .to_string();

        Ok(RsFile { path, content })
    }

    fn extensions(&self) -> &[&str] {
        &["rs"]
    }
}
