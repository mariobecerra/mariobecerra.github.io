---
layout: page
title: Contact
permalink: /contact/
---

<!-- <div id="contact">
  <div id="contact-form">
          <form action="https://formspree.io/xoqpqdnj" method="POST">
          <input type="hidden" name="_subject" value="Contact request from personal website" />
          <input type="email" name="_replyto" placeholder="Your email" required>
          <textarea name="message" placeholder="Type your message" required></textarea>
          <button type="submit">Send</button>
      </form>
  </div>
</div> -->

Want to have a chat with me? Want to hire me for a freelance job? I can help you out! Just send me a message with your email address an I'll contact you as soon as possible.

<form
  action="https://formspree.io/xoqpqdnj"
  method="POST"
>
  <label>
    Your email:
    <br>
    <input type="text" name="_replyto">
  </label>
  <br>
  <label>
    <br>
    Your message:
    <br>
    <textarea name="message"></textarea>
  </label>
  <br>
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

<br>
<br>
<br>