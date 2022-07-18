# org-impaste

org-impaste 是一个用 Rust 语言开发的 Emacs 插件。
其特性包含

1. [x] 只含有一个 Rust 编译的链接库，不需要 curl, imagemagick 等依赖。
1. [x] 使用多线程技术，不会在网络不畅时因为下载图片而卡住整个 Emacs。
1. [x] 读取环境变量 `HTTP_PROXY` 以及 `HTTPS_PROXY` 来指定网络代理
1. [ ] 使用 sha256 hex 命名管理图片文件，在设置中指定图片目录路径，并在同目录下以 `<image filename>.link.toml` 文件存储图片的下载来源以及图床地址。
1. [ ] 配置集成 PicGo，自动保存图片上传后的地址。
1. [ ] 调用外部程序修改图片，遵守 COW 原则，在修改图片后生成新的副本保存。
1. [ ] 简单的管理功能，搜索 org 目录下的所有 org 文档中的图片链接，清理孤儿图片。

一个 .link.toml 文件的范例：

``` toml
# .link.toml
[origin]
# 这个图片是从哪里下载的
links = ["https://example.org/files/example.png"]
[upload]
# 这个图片上传到哪里了
links = ["https://image-bed.org/files/<sha256 hex>.png"]
```

该插件提供以下命令：

1. [x] `org-impaste-download` 下载指定链接的图像文件，存储在指定位置，并在光标处插入链接。
1. [x] `org-impaste-clipboard` 从剪贴板获取图像文件，存储在指定位置，并在光标处插入链接。
1. [ ] 接受拖曳操作，下载网络图像或复制本地图像，并在光标处插入链接。
1. [ ] `org-impaste-manage` 打开 `*org-impaste-manager*` buffer，查看各图片的引用情况。
1. [ ] `org-impaste-clean` 清理引用计数为 0 的孤儿图片

# 安装与配置

一个参考配置为

```lisp
(use-package org-impaste
  :after org
  :init (setq org-impaste-storage-dir (file-truename "~/org/images/"))
  :bind (("C-c i d" . org-impaste-download)
         ("C-c i p" . org-impaste-clipboard)))
```

TODO: 自动下载二进制依赖或自动编译

## Doom Emacs

```lisp
;; packages.el
(package! org-impaste
  :recipe (:host github :repo "zombie110year/org-impaste))

;; config.el
(use-package! org-impaste
  :after org
  :config
    (setq org-impaste-storage-dir (file-truename "~/org/images"))
    (define-key org-mode-map "C-c i d" org-impaste-download)
    (define-key org-mode-map "C-c i p" org-impaste-clipboard))
```

# 试用

需要安装 rust 工具链。

```
cd org-impaste
cargo build --release
mv target/release/org_impaste_module.dll ./
cargo clean
```

或者到 [GitHub Releases](https://github.com/zombie110year/org-impaste/releases) 下载后将链接库文件保存到对应位置。

使用 emacs 打开 [./example.org](./example.org) 文件，尝试运行 `org-impaste-download` 与 `org-impaste-clipboard` 命令。

# 感谢

+ abo-abo/org-download <https://github.com/abo-abo/org-download>
+ 用 Rust 扩展 Emacs 功能 <https://cireu.github.io/2020/04/05/rust-emacs-module/>
