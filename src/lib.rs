pub(crate) mod curl;

use anyhow::Error;
use curl::Curl;
use emacs::{defun, IntoLisp};
use sha2::{Digest, Sha256};
use std::{io::Write, path::PathBuf};
emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "org-impaste")]
fn init(_: &emacs::Env) -> emacs::Result<()> {
    Ok(())
}

/// download image from internet
///
/// + url : image's url.
/// + store : the directory to store image files.
/// + referer : set it if return 403 error without referer.
#[defun(name = "-download-external")]
fn download(
    env: &emacs::Env,
    url: String,
    store: String,
    referer: String,
) -> emacs::Result<emacs::Value<'_>> {
    let impath = download_(url, PathBuf::from(store), referer)?;
    return impath.into_lisp(&env);
}

fn download_(url: String, store: PathBuf, referer: String) -> Result<String, Error> {
    let curl = Curl::new().check_installed()?.default_options();
    let curl = if !referer.is_empty() {
        curl.referer(&referer)
    } else {
        curl
    };
    let im = curl.get(&url)?;
    let filename = hex_filename(&im);
    let mime = tree_magic_mini::from_u8(&im[..]);
    let fileext = mime.split("/").last().unwrap();
    let impath = store.join(format!("{filename}.{fileext}"));
    let infopath = store.join(format!("{filename}.txt"));
    std::fs::write(&impath, im)?;
    let mut log = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&infopath)?;
    log.write(url.as_bytes())?;
    log.write(b"\n")?;
    return Ok(impath.to_str().unwrap().to_string());
}

/// paste image from clipboard
#[defun(name = "-clipboard-external")]
fn clipboard(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let log = format!("org-impaste-clipboard");
    env.message(&log)
}

pub(crate) fn hex_filename(content: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(content);
    let hash = hasher.finalize();
    let hexname = format!("{:x}", hash);
    return hexname;
}

#[test]
fn test_download() {
    let x = download_(
        "https://tse1-mm.cn.bing.net/th/id/OIP-C.iDdvnaGvywxqBKurXHDKcAHaFj?pid=ImgDet&rs=1"
            .to_string(),
        PathBuf::new().join("debug"),
        "".to_string(),
    )
    .unwrap();
    println!("{x}");
}
