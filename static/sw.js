const VERSION = "v4";
const PRECACHE = [
  "./",
  "/app.css",
  "/index.html",
  "/normalize.min.css",
  "/open-props.min.css",
  "/theme.dark.switch.min.css",
  "/theme.light.switch.min.css",
  "/registerServiceWorker.js",

  "/manifest.json",
  "/icons/apple-touch-icon.png",
  "/icons/favicon-16x16.png",
  "/icons/favicon-32x32.png",
  "/favicon.ico",

  "/icons/android/android-launchericon-144-144.png",
  "/icons/android/android-launchericon-192-192.png",
  "/icons/android/android-launchericon-48-48.png",
  "/icons/android/android-launchericon-512-512.png",
  "/icons/android/android-launchericon-72-72.png",
  "/icons/android/android-launchericon-96-96.png",
  "/icons/ios/100.png",
  "/icons/ios/1024.png",
  "/icons/ios/114.png",
  "/icons/ios/120.png",
  "/icons/ios/128.png",
  "/icons/ios/144.png",
  "/icons/ios/152.png",
  "/icons/ios/16.png",
  "/icons/ios/167.png",
  "/icons/ios/180.png",
  "/icons/ios/192.png",
  "/icons/ios/20.png",
  "/icons/ios/256.png",
  "/icons/ios/29.png",
  "/icons/ios/32.png",
  "/icons/ios/40.png",
  "/icons/ios/50.png",
  "/icons/ios/512.png",
  "/icons/ios/57.png",
  "/icons/ios/58.png",
  "/icons/ios/60.png",
  "/icons/ios/64.png",
  "/icons/ios/72.png",
  "/icons/ios/76.png",
  "/icons/ios/80.png",
  "/icons/ios/87.png",
];

const addResourcesToCache = async (resources) => {
  const cache = await caches.open(VERSION);
  await cache.addAll(resources);
};

const putInCache = async (request, response) => {
  const cache = await caches.open(VERSION);
  await cache.put(request, response);
};

const cacheFirst = async ({ request, preloadResponsePromise, fallbackUrl }) => {
  // First try to get the resource from the cache
  const responseFromCache = await caches.match(request);
  if (responseFromCache) {
    return responseFromCache;
  }

  // Next try to use the preloaded response, if it's there
  // NOTE: Chrome throws errors regarding preloadResponse, see:
  // https://bugs.chromium.org/p/chromium/issues/detail?id=1420515
  // https://github.com/mdn/dom-examples/issues/145
  // To avoid those errors, remove or comment out this block of preloadResponse
  // code along with enableNavigationPreload() and the "activate" listener.
  // const preloadResponse = await preloadResponsePromise;
  // if (preloadResponse) {
  //   console.info("using preload response", preloadResponse);
  //   putInCache(request, preloadResponse.clone());
  //   return preloadResponse;
  // }

  // Next try to get the resource from the network
  try {
    const responseFromNetwork = await fetch(request.clone());
    // response may be used only once
    // we need to save clone to put one copy in cache
    // and serve second one
    if (request.url.startsWith(self.location.origin)) {
      putInCache(request, responseFromNetwork.clone());
    }
    return responseFromNetwork;
  } catch (error) {
    const fallbackResponse = await caches.match(fallbackUrl);
    if (fallbackResponse) {
      return fallbackResponse;
    }
    // when even the fallback response is not available,
    // there is nothing we can do, but we must always
    // return a Response object
    return new Response("Network error happened", {
      status: 408,
      headers: { "Content-Type": "text/plain" },
    });
  }
};

// const enableNavigationPreload = async () => {
//   if (self.registration.navigationPreload) {
//     // Enable navigation preloads!
//     await self.registration.navigationPreload.enable();
//   }
// };

self.addEventListener("activate", (event) => {
  // event.waitUntil(enableNavigationPreload());
  const currentCaches = [PRECACHE];
  event.waitUntil(
    caches
      .keys()
      .then((cacheNames) =>
        cacheNames.filter((cacheName) => !currentCaches.includes(cacheName)),
      )
      .then((cachesToDelete) => {
        return Promise.all(
          cachesToDelete.map((cacheToDelete) => {
            return caches.delete(cacheToDelete);
          }),
        );
      })
      .then(() => self.clients.claim()),
  );
});

self.addEventListener("install", (event) => {
  event.waitUntil(addResourcesToCache(PRECACHE));
  self.skipWaiting();
});

self.addEventListener("fetch", (event) => {
  event.respondWith(
    cacheFirst({
      request: event.request,
      preloadResponsePromise: event.preloadResponse,
      fallbackUrl: "index.html",
    }),
  );
});
