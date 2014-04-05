# PHP Extras

A small collection of extra features for Emacs `php-mode`.

Currently includes:

  * `php-extras-insert-previous-variable`
  * `php-extras-eldoc-documentation-function`
  * Auto complete source for PHP functions based on
    `php-extras-eldoc-documentation-function`
  * Company completion back-end for PHP functions based on
    `php-extras-eldoc-documentation-function`


## `php-extras-insert-previous-variable`

When variable names get too long or you have to juggle a lot of nested
arrays it gets cumbersome to repeat the same variables over and over
again while programming.

In example you have the code below and want to debug what value you
actually parsed to `some_function()`. You have point at `^` and now
all you have to write is repeat the variable...

```php
some_function($my_array['some_level'][0]['another_level'][7]);
print_r(^);
```

Enter `php-extras` and you just hit <kbd>C-c C-$</kbd> and it will
insert the previous variable (including array indexes).

If you prefix the command (i.e. <kbd>C-u 3 C-c C-$</kbd>) it will
search back 3 variables and with negative prefix arguments it will
search forward.


## `php-extras-eldoc-documentation-function`

`eldoc-mode` is a nice minor mode that ships with Emacs. It will
display a function tip in the mini buffer showing the function and its
arguments for the function at point. That is if you provide a function
to look up the function definition.

`php-extras` provides such a function for looking up all the core PHP
functions.

The function `php-extras-generate-eldoc` will download the
[PHP function list](http://doc.php.net/downloads/json/php_manual_en.json)
and extract the function definitions (slow) and store them in a hash
table on disk for you.

If you install `php-extras` as an ELPA package from
[Marmalade](http://marmalade-repo.org/packages/php-extras) the hash
table is already generated for you.


## Auto complete source for PHP functions based

The PHP functions extracted for
`php-extras-eldoc-documentation-function` is also setup as a source for
[auto-complete](http://cx4a.org/software/auto-complete).

[auto-complete](http://cx4a.org/software/auto-complete) already comes
with a dictionary of PHP functions and will auto complete on them
using the `ac-source-dictionary`.

The source we provide with `php-extras` will hopefully be more up to
date.


## Company completion back-end for PHP functions based

Users of [company-mode](http://company-mode.github.io/) will also get
in-buffer  completion based on the extracted PHP functions.


## Installation

The easiest way to install `php-extras` is probably to install it via
the ELPA archive at
[Marmalade](http://marmalade-repo.org/packages/php-extras).

ELPA (package.el) is part of Emacs 24. For Emacs 23 see
[Marmalade](http://marmalade-repo.org) for installation instructions.

The version number of the ELPA package will have the date appended
when the package was build and hence the date the documentation got
extracted from [php.net](http://php.net).


### Manual installation

I really recommend that you install this package via ELPA as
described above.

If you insist on installing it manually try to follow this recipe:

* Place the folder with the files somewhere on your disk.

* Add this to your `.emacs` / `.emacs.d/init.el`:

```lisp
(add-to-list 'load-path "/somewhere/on/your/disk/php-extras")
(eval-after-load 'php-mode
  (require 'php-extras))
```

* Either restart your Emacs or evaluate the `add-to-list` expression.

* Generate the hash table containing the PHP functions:

   <kbd>M-x load-library RET php-extras-gen-eldoc RET</kbd>

   <kbd>M-x php-extras-generate-eldoc RET</kbd>


## Development of PHP Extras

PHP Extras is developed at
[GitHub](https://github.com/arnested/php-extras).  Feature requests,
ideas, bug reports, and pull request are more than welcome!
