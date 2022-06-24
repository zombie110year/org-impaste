# org-impaste

org-impaste 是一个用 Rust 语言开发的 Emacs 插件。
其功能包含

1. [ ] 下载指定链接的图像文件，存储在指定位置，并在光标处插入链接
1. [ ] 从剪贴板获取图像文件，存储在指定位置，并在光标处插入链接
1. [ ] 接受拖曳操作，下载网络图像或复制本地图像，并在光标处插入链接

另外，还可以进行精细化的设置：

1. [x] 设置存储位置，可指定目录路径
1. [x] 按 sha256 hex 字符串命名文件，在同名的 .txt 文件中存储每一次下载的原始链接
1. [x] 读取环境变量 `HTTP_PROXY` 以及 `HTTPS_PROXY` 来指定网络代理

# 安装

# 配置

一个参考配置为

```lisp
(use-package org-impaste
  :init (setq org-impaste-storage-dir (file-truename "~/org/images/"))
  :bind (("C-c i d" . org-impaste-download)
         ("C-c i p" . org-impaste-clipboard)))
```

# 感谢

+ abo-abo/org-download <https://github.com/abo-abo/org-download>
+ 用 Rust 扩展 Emacs 功能 <https://cireu.github.io/2020/04/05/rust-emacs-module/>
