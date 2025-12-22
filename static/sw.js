const VERSION = "v2";
const CACHE = [
  "./",
  "/app.css",
  // "/assets/",
  "/index.html",
  "/normalize.min.css",
  "/open-props.min.css",
  "/theme.dark.switch.min.css",
  "/theme.light.switch.min.css",
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
    console.log("RQ!", request, request.url.startsWith(self.location.origin));
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
  const currentCaches = [CACHE];
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
  event.waitUntil(addResourcesToCache(CACHE)).then(self.skipWaiting());
});

self.addEventListener("fetch", (event) => {
  console.log("FETCH", event);
  event.respondWith(
    cacheFirst({
      request: event.request,
      preloadResponsePromise: event.preloadResponse,
      fallbackUrl: "index.html",
    }),
  );
});
