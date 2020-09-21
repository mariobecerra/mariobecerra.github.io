---
layout: page
title: Contact
permalink: /contact/
---

<p><u>Email:</u> mbecerracontreras@gmail.com</p>
<p><u>Phone number:</u> +52 55 1513 0162</p>
<p><u>Location:</u> Leuven, Belgium</p>

<div id="contact">
        <h2>Send me a message</h2>
        <div id="contact-form">
                <form action="https://formspree.io/xoqpqdnj" method="POST">
                <input type="hidden" name="_subject" value="Contact request from personal website" />
                <input type="email" name="_replyto" placeholder="Your email" required>
                <textarea name="message" placeholder="Type your message" required></textarea>
                <button type="submit">Send</button>
            </form>
        </div>
    </div>

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

