# TidBit (NLP)
> NLP Microservice for Tidbit

TidBit is a Chrome extension that allows the user to turn any web content into a simplified view-article of the most important pieces of content along with the most important sections.

This microservice handles receiving a webpage HTML chunk, analyzing the key sections and content on that piece of context and then returning back to the end user, a Markdown document simplication of the entire webpage.

## Tech Stack
* 🖥️ **Haskell** - main programming language of choice
  * **Snap** - Haskell-based web server
* ⚡ **Natural Language Processing**
  * **Parse** text content into Section Trees (ST), based on the DOM tree structure.
  * **Map** over each Section Tree into a Flattened Section Trees where at the very first instance we hit some text body, any other child nested under that text, is categorized along with that text content. We are essentially flattening the hierarchy of Section Trees.
  * **Reduce** the list of Section Trees by running **importance determination algorithms (IDA)**, eradicating those that are very dissimilar from the main topic of the sentence.
    * Cosine Similary Distance Computation
    * BFS Hierarchy Computation
  * **Construct** a list of Section Blocks (SB), each with a body text and title, from the reduced list of section trees
  * **Transform** each Section Block into a Markdown Block (using Pandoc), which are then all concatenated to a string.


## API Endpoints

All the API endpoints are accessible under the prefix `<hostname>/api/`. All the API outputs are of the form:

```json
{
  "status": "<int>",
  "message": "<string>",
  "data": "<ApiEndpointOutput>"
}
```

1. `/tidbit`
  * Given a sentence, find key topics, form "card" objects around those important key terms and define those key topics in that sentence.
    * **POST**
    * Accepts **string**
    * Returns **string**
