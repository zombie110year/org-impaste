pub(crate) mod commands;
pub(crate) mod prelude;
pub(crate) mod tools;

use emacs::{defun, IntoLisp};
use std::path::PathBuf;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "org-impaste-module")]
fn init(_: &emacs::Env) -> emacs::Result<()> {
    Ok(())
}

/// Download image from internet
///
/// + url : image's url.
/// + store : the directory to store image files.
/// + referer : set it if return 403 error without referer.
#[defun(name = "-download")]
fn download(
    env: &emacs::Env,
    url: String,
    store: String,
    referer: String,
) -> emacs::Result<emacs::Value<'_>> {
    let impath = prelude::download(url, PathBuf::from(store), referer)?;
    return impath.into_lisp(&env);
}

/// Paste image from clipboard
///
/// + store : the directory to store image files.
#[defun(name = "-clipboard")]
fn clipboard(env: &emacs::Env, store: String) -> emacs::Result<emacs::Value<'_>> {
    let impath = prelude::clipboard(PathBuf::from(store))?;
    return impath.into_lisp(env);
}

/// Generate a hex string by system time(nano seconds) as a placeholder
#[defun(name = "-timer-key")]
fn timer_key(env: &emacs::Env) -> emacs::Result<emacs::Value<'_>> {
    let key = prelude::timer_hex()?;
    key.into_lisp(env)
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;
    use std::path::PathBuf;

    #[test]
    fn test_download() {
        let x = download(
            "http://i2.hdslb.com/bfs/archive/e909738be019b92f7039ad204ef1437ffe54a6c4.jpg"
                .to_string(),
            PathBuf::new().join("debug"),
            "https://www.bilibili.com/".to_string(),
        )
        .unwrap();
        println!("{x}");
    }

    #[test]
    fn test_clipboard() {
        let x = clipboard(PathBuf::from("debug")).unwrap();
        println!("{x}");
    }

    #[test]
    fn test_random_key() {
        let x = timer_hex().unwrap();
        println!("{x}");
    }
}
