importScripts("./protosearch.js")

async function getQuerier() {
  let querier = fetch("./searchIndex.idx")
    .then(res => res.blob())
    .then(blob => QuerierBuilder.load(blob))
    .catch((error) => console.error("getQuerier error: ", error));
  return await querier
}
const querierPromise = getQuerier()

async function searchIt(query) {
  const querier = await querierPromise
  return querier.search(query)
}

onmessage = async function(e) {
  const query = e.data || '' // empty strings become undefined somehow ...
  this.postMessage(await searchIt(query))
}

searchIt("warmup")
