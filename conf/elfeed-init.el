;;; elfeed-init.el -- Emacs Settings
;;; Commentary:
;;; Code:

(autoload 'elfeed "elfeed" nil t)
(eval-after-load "elfeed"
  '(progn
     (global-set-key (kbd "C-x w") 'elfeed)
     (setq elfeed-feeds
           '(;; Security
             ("http://vrda.jpcert.or.jp/feed/ja/atom.xml") ;; VRDA Security
             ("http://jvndb.jvn.jp/ja/rss/jvndb_new.rdf") ;; JVNDB Security
             ;; Linux, Debian, Free Software Foundation
             ("https://www.debian.org/News/news")
             ("https://www.debian.org/security/dsa")
             ("https://static.fsf.org/fsforg/rss/news.xml")
             ("https://static.fsf.org/fsforg/rss/blogs.xml")
             ("https://www.linux.com/feeds/news/rss")
             ("https://www.w3.org/blog/news/feed")
             ;; Tech
             ("https://thinkit.co.jp/rss.xml") ;; ThinkIT
             ("http://postd.cc/feed/") ;; PostD
             ("https://codeiq.jp/magazine/feed/") ;; CodeIQ
             ("http://rss.rssad.jp/rss/codezine/new/20/index.xml") ;; Codezine
             ("https://geechs-magazine.com/feed") ;; Geechs
             ("http://uxmilk.jp/feed") ;;Uxmilk
             ("http://jp.techcrunch.com/feed/") ;; Techcrunch
             ("http://www.seleqt.net/feed/") ;; Seleqt
             ("http://feeds.feedburner.com/WebmasterCentral?format=xml") ;; Google Web-Master Official Blog
             ("https://blogs.msdn.microsoft.com/bingdevcenter/feed/") ;; Bing Developer Blog
             ("https://hacks.mozilla.org/feed/") ;; Mozilla Hacks
             ("http://wired.jp/rssfeeder/") ;; Wired
             ("https://liginc.co.jp/feed") ;; LIG
             ("https://ferret-plus.com/.rss") ;; Ferret
             ("https://news.ycombinator.com/rss") ;; Hacker News
             ("https://www.infoq.com/jp/feed?token=6He6dTMXb4uv4glWb5XjKb3YeU0sB0QV") ;; InfoQ
             ("http://rss.rssad.jp/rss/itmnews/2.0/news_bursts.xml") ;; IT Media
             ("http://rss.rssad.jp/rss/itmnews/2.0/news_security.xml") ;; IT Media
             ("http://feeds.japan.cnet.com/rss/cnet/all.rdf") ;; CNET
             ("http://b.hatena.ne.jp/hotentry/it.rss") ;; Hatena1
             ("http://b.hatena.ne.jp/entrylist/it.rss") ;; Hatena2
             ("http://gihyo.jp/dev/feed/rss2") ;; Gihyo
             ;; /.
             ("https://srad.jp/slashdot.rss")
             ("https://srad.jp/linux.rss")
             ("https://srad.jp/developers.rss")
             ("https://srad.jp/opensource.rss")
             ("https://srad.jp/mobile.rss")
             ("https://srad.jp/it.rss")
             ("https://srad.jp/apple.rss")
             ("https://srad.jp/security.rss")))))
