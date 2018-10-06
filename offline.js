importScripts('https://storage.googleapis.com/workbox-cdn/releases/3.6.1/workbox-sw.js');

workbox.routing.registerRoute(
  new RegExp('.*'),
  workbox.strategies.networkFirst()
);
