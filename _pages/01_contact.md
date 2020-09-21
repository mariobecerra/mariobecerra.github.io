---
layout: page
title: Contact
permalink: /contact/
---

<p><u>Email:</u> mbecerracontreras@gmail.com</p>
<p><u>Phone number:</u> +52 55 1513 0162</p>
<p><u>Location:</u> Leuven, Belgium</p>

<form
  action="https://formspree.io/xoqpqdnj"
  method="POST"
>
  <label>
    Your email:
    <input type="text" name="_replyto">
  </label>
  <label>
    Your message:
    <textarea name="message"></textarea>
  </label>

  <!-- your other form fields go here -->

  <button type="submit">Send</button>
</form>

## Around the web

{% if site.github_username %}
  <li>
    <a href="https://github.com/{{ site.github_username }}">
      <i class="fa fa-github"></i> GitHub
    </a>
  </li>
{% endif %}

{% if site.linkedin_username %}
  <li>
    <a href="https://linkedin.com/in/{{ site.linkedin_username }}">
      <i class="fa fa-linkedin"></i> LinkedIn
    </a>
  </li>
{% endif %}

