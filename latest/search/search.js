function renderDoc(hit) {
  const path = hit.fields.path
  const link = "../" + hit.fields.path.replace(".txt", ".html")
  const title = hit.highlights["title"] || hit.fields["title"]
  const preview = hit.highlights["body"]
  return (
`
<ol>
  <div class="card">
    <div class="card-content">
      <p class="is-size-6 has-text-grey-light">
        <span>${path}</span>
      </p>
      <div class="level-left">
        <p class="title is-capitalized is-flex-wrap-wrap">
          <a href="${link}" target="_blank">
            <span>${title}</span>
          </a>
        </p>
      </div>
      <p class="subtitle">${preview}</p>
    </div>
  </div>
</ol>
`
  )
}
function renderScaladoc(hit) {
  const title = hit.fields.functionName
  const description = hit.fields.description
  const returnType = hit.fields.returnType
  const params = hit.fields.params
  return (
`
<ol>
  <div class="card">
    <div class="card-content">
      <div class="level-left">
        <p class="title is-capitalized is-flex-wrap-wrap">
          <span>${title}</span>
        </p>
      </div>
      <p class="subtitle">${description}</p>
      <p class="subtitle">Parameters: ${params}</p>
      <p class="subtitle">Return type: ${returnType}</p>
    </div>
  </div>
</ol>
`
  )
}

async function main() {
  var app = document.getElementById("app")
  var searchBar = document.getElementById("search_input")
  const urlParams = new URLSearchParams(location.search)

  const renderFunction = urlParams.get("type") == "scaladoc" ? renderScaladoc : renderDoc

  const maybeIndex = urlParams.get("index")
  const workerJS = maybeIndex ? `worker.js?index=${maybeIndex}` : "worker.js"

  const worker = new Worker(workerJS)
  worker.onmessage = function(e) {
    const markup = e.data.map(renderFunction).join("\n")
    app.innerHTML = markup
  }

  searchBar.addEventListener('input', function () {
    worker.postMessage(this.value)
  })
}
main()
