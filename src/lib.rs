pub(crate) mod curl;

use curl::Curl;
use emacs::{defun, IntoLisp};
use sha2::{Digest, Sha256};
use std::path::PathBuf;

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
    let curl = Curl::new().check_installed()?.default_options();
    let curl = if !referer.is_empty() {
        curl.referer(&referer)
    } else {
        curl
    };
    let im = curl.get(&url)?;
    let filename = hex_filename(&im);
    // todo 如果不能直接从 url 中获取后缀，则使用文件头推测
    let fileext = url.split('.').last().unwrap();
    let mut name = String::new();
    name.push_str(&filename);
    name.push('.');
    name.push_str(&fileext);
    std::fs::write(PathBuf::from(store).join(&name), im)?;
    return name.into_lisp(&env);
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
