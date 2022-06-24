use anyhow::Error;
use emacs::{defun, IntoLisp};
use reqwest::blocking::Response;
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
#[defun(name = "-download-external")]
fn download(env: &emacs::Env, url: String, store: String) -> emacs::Result<emacs::Value<'_>> {
    let resp = fetch(&url)?;
    let im = resp.bytes()?.to_vec();
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

pub(crate) fn fetch(url: &String) -> Result<Response, Error> {
    let client = reqwest::blocking::ClientBuilder::default()
        .referer(true)
        .user_agent(
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:102.0) Gecko/20100101 Firefox/102.0",
        )
        .build()?;
    let req = client.get(url).build()?;
    let resp = client.execute(req)?;
    Ok(resp)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_fetch() {
        let url = "https://o.acgpic.net/img-original/img/2022/06/19/00/00/13/99142922_p0.jpg";
        let resp = fetch(&url.to_string()).unwrap();
        std::fs::write("./target/test.jpg", resp.bytes().unwrap()).unwrap();
    }
}
