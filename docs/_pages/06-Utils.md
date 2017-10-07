---
layout: page
title: Utils
class: section
---

You'll find this post in your `_posts` directory - edit this post and re-build (or run with the `-w` switch) to see your changes!
To add new posts, simply add a file in the `_posts` directory that follows the convention: YYYY-MM-DD-name-of-post.ext.

Jekyll also offers powerful support for code snippets:

{% highlight fortran %}
subroutine String_test()
	type(String) :: str1
	type(String) :: str2
	type(String) :: str3
	integer :: int1
	real(8) :: real1
	complex(8) :: complex1
	character(100), allocatable :: fstrArray(:)
	integer, allocatable :: intArray(:)
	real(8), allocatable :: realArray(:)
	character(:), allocatable :: fstr1
	character(100), allocatable :: tokens(:)
! 		character(100) :: fstr
	character(:), allocatable :: fstr
	integer :: i
	
	write(*,*)
	write(*,*) "Testing constructors"
	write(*,*) "===================="

	call str1.init( "Hello my friends" )
	str2 = str1
	call str2.show()
	
	str2 = "Hello my friends from asignation operator"
	call str2.show()
	
	fstr = "Hello my friends from fortran string"
	str2 = fstr
	call str2.show()
{% endhighlight %}

Check out the [Jekyll docs][jekyll] for more info on how to get the most out of Jekyll. File all bugs/feature requests at [Jekyll's GitHub repo][jekyll-gh].

[jekyll-gh]: https://github.com/mojombo/jekyll
[jekyll]:    http://jekyllrb.com
