use anyhow::Error;
use std::path::PathBuf;
use std::process::Command;

/// # example
///
/// ```rust,ignore
/// let content = Curl::new()
///     .check_installed()?
///     .default_options()
///     .get("https://example.org");
/// ```
#[derive(Debug)]
pub(crate) struct Curl {
    program: String,
    args: Vec<String>,
}

impl Curl {
    pub fn new() -> Self {
        #[cfg(target_os = "windows")]
        let cmd = "curl.exe";
        #[cfg(target_os = "unix")]
        let cmd = "curl";
        Self {
            program: cmd.to_string(),
            args: Vec::with_capacity(4),
        }
    }

    pub fn check_installed(self) -> Result<Self, Error> {
        let path = std::env::var("PATH")?;

        #[cfg(target_os = "windows")]
        let sep = ";";
        #[cfg(target_os = "unix")]
        let sep = ":";

        let curl_exists = path
            .split(sep)
            .map(|p| {
                let p = PathBuf::from(p);
                let b = p.join(&self.program);
                b
            })
            .map(|p| p.exists())
            .any(|b| b == true);
        if curl_exists {
            Ok(self)
        } else {
            Err(anyhow::anyhow!(
                "FileNotFound: {}, install or add it to PATH.",
                &self.program
            ))
        }
    }

    /// 1. curl slient
    /// 2. curl fail slient
    /// 3. curl follow redirection
    /// 4. curl use Firefox's User-Agent
    /// 5. set proxy by environment vars 'HTTP_PROXY'
    pub fn default_options(mut self) -> Self {
        self.args.push("-sfL".to_string());
        self.args.push("-A".to_string());
        self.args.push(
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:102.0) Gecko/20100101 Firefox/102.0"
                .to_string(),
        );

        if let Ok(http_proxy) = std::env::var("HTTP_PROXY") {
            self.args.push("-x".to_string());
            self.args.push(http_proxy);
        }

        self
    }

    /// set referer
    pub fn referer(mut self, url: &str) -> Self {
        self.args.push("-e".to_string());
        self.args.push(url.to_string());
        self
    }

    /// set proxy, if proxy has been set in `default_options`, it will be replace.
    #[allow(dead_code)]
    pub fn proxy(mut self, purl: &str) -> Self {
        let arg = format!("-x \"{purl}\"");
        let arg_exists: Vec<bool> = self.args.iter().map(|a| a == "-x").collect();
        if let Ok(i) = arg_exists.binary_search(&true) {
            // remove `-x` `xxx`
            self.args.remove(i);
            self.args.remove(i);
        }
        self.args.push(arg);
        self
    }

    pub fn get(&self, url: &str) -> Result<Vec<u8>, Error> {
        let out = Command::new(self.program.as_str())
            .args(&self.args)
            .arg(url)
            .output()?;
        Ok(out.stdout.to_owned())
    }
}

#[cfg(test)]
#[test]
fn test_curl() {
    let x = Curl::new()
        .check_installed()
        .unwrap()
        .default_options()
        .referer("https://pixivic.com/");
    let b = x
        .get("https://o.acgpic.net/img-original/img/2022/06/19/00/00/13/99142922_p0.jpg")
        .unwrap();
    let fheader: String = b[..0x10]
        .iter()
        .map(|cc| {
            if cc.is_ascii_graphic() {
                *cc as char
            } else {
                '.'
            }
        })
        .collect();

    println!("File Header: {}", fheader);
    std::fs::write("./debug_curl.jpg", &b).unwrap();
}
