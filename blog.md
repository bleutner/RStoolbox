---
layout: page
title: Blog 
header: Pages
---
{% include JB/setup %}

<ul class="posts">  
  {% for post in site.posts %}
    <li><h1 class="entry-title">
	  <a href="{{ BASE_PATH }}{{ post.url }}">{{post.title}}</a>
	  </h1>
      <p class="blogdate">{{ post.date | date: "%d %B %Y" }}</p>
	  <div class="entry-content"></div>
      <div>{{ post.content |truncatehtml | truncatewords: 60 }}</div>
     </li>
	{% endfor %}  
</ul>

