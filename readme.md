
# IRisk Data Visualization Dashboard

A prototype application written in [Racket][racket] used to experiment with
data visualization.  A full description is available in [this blog
post][blog-post], but this code has now been updated and it has some extra
features from what is described there.

![](https://alex-hhh.github.io/img/a022/irisk.gif)

# Building the application

You will need [Racket][racket] installed and also the [data-frame][df] and
[gui-widget-mixins][gwm] packages, these can be installed using the command:

```
raco pkg install data-frame
raco pkg install gui-widget-mixins
```

You can run the application from DrRacket by opening the "main.rkt" file and
run it, but you can also build a standalone executable.  On Windows platform
you can use the command below, and there are similar commands to build MacOS
or Linux executables.

```
raco exe --gui --embed-dlls --ico irisk-icon.ico -o AL2-IRisk.exe main.rkt
```

The command below will bundle the distribution files, with the resulting
application in the "AL2-IRisk" folder and can be run on a machine which does
need to have Racket installed.

```
raco distribute ./AL2-IRisk AL2-IRisk.exe
```

The icon is from [IconArchive][meld-icon], where you can download other file
formats of this icon (or any other one) which can be used on MacOS or Linux.

[blog-post]: https://alex-hhh.github.io/2019/02/data-visualization-dashboard.html
[meld-icon]: http://www.iconarchive.com/show/papirus-apps-icons-by-papirus-team/meld-icon.html
[df]: https://pkgs.racket-lang.org/package/data-frame
[gwm]: https://pkgd.racket-lang.org/pkgn/package/gui-widget-mixins
[racket]: https://www.racket-lang.org
