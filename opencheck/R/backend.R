#' An OpenAI-compatible chat backend with logprobs
#'
#' Works with vLLM's OpenAI server, Ollama's `/v1` endpoint, OpenRouter and
#' the OpenAI API. Requests first-token logprobs so classification
#' probabilities come from the model, not from self-reported confidence.
#'
#' @param base_url API root, e.g. `"http://127.0.0.1:8000/v1"`.
#' @param model Model name as the server expects it.
#' @param api_key_env Name of the environment variable holding the API key,
#'   or `NULL` for local servers.
#' @param temperature,seed,max_tokens,top_logprobs Decoding settings; recorded
#'   in the cache and the lock.
#' @param extra Named list of extra request fields (e.g.
#'   `list(chat_template_kwargs = list(enable_thinking = FALSE))`).
#' @param timeout Seconds per request.
#' @param retries Attempts on HTTP 429 or 5xx.
oc_backend_openai <- function(base_url, model, api_key_env = NULL,
                              temperature = 0, seed = 42L, max_tokens = 5L,
                              top_logprobs = 10L, extra = list(),
                              timeout = 120, retries = 4L) {
  settings <- list(model = model, temperature = temperature, seed = seed,
                   max_tokens = max_tokens, top_logprobs = top_logprobs,
                   extra = extra)
  url <- paste0(sub("/+$", "", base_url), "/chat/completions")
  call <- function(messages) {
    body <- c(list(model = model, messages = messages, seed = seed,
                   logprobs = TRUE, top_logprobs = top_logprobs), extra)
    if (!is.null(temperature)) body$temperature <- temperature   # NULL: model default
    # Newer OpenAI models reject max_tokens in favour of max_completion_tokens.
    if (grepl("api\\.openai\\.com", base_url)) body$max_completion_tokens <- max_tokens
    else body$max_tokens <- max_tokens
    key <- if (is.null(api_key_env)) "" else Sys.getenv(api_key_env)
    if (!is.null(api_key_env) && !nzchar(key)) {
      stop("Environment variable ", api_key_env, " is not set.", call. = FALSE)
    }
    res <- http_post_json(url, body, key, timeout, retries)
    parse_chat_response(res)
  }
  structure(list(id = model, settings = settings,
                 settings_hash = hash_object(settings), call = call,
                 base_url = base_url),
            class = "oc_backend")
}

#' A backend for tests and dry runs
#'
#' @param fun A function taking the message list and returning
#'   `list(content = <string>, logprobs = <data frame or NULL>)`. The logprobs
#'   data frame has one row per alternative for the first token, with columns
#'   `token` and `logprob`.
#' @param id A model id to record.
oc_backend_mock <- function(fun, id = "mock") {
  structure(list(id = id, settings = list(model = id),
                 settings_hash = hash_object(list(model = id)), call = fun,
                 base_url = "mock"),
            class = "oc_backend")
}

http_post_json <- function(url, body, key, timeout, retries) {
  payload <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null",
                              digits = NA)
  for (attempt in seq_len(retries)) {
    h <- curl::new_handle()
    headers <- list("Content-Type" = "application/json")
    if (nzchar(key)) headers$Authorization <- paste("Bearer", key)
    do.call(curl::handle_setheaders, c(list(h), headers))
    curl::handle_setopt(h, postfields = enc2utf8(as.character(payload)),
                        timeout = timeout)
    r <- tryCatch(curl::curl_fetch_memory(url, handle = h),
                  error = function(e) e)
    if (inherits(r, "error")) {
      if (attempt == retries) stop("Request failed: ", conditionMessage(r),
                                   call. = FALSE)
    } else if (r$status_code == 200) {
      txt <- rawToChar(r$content)
      Encoding(txt) <- "UTF-8"
      return(jsonlite::fromJSON(txt, simplifyVector = FALSE))
    } else if (!(r$status_code == 429 || r$status_code >= 500) ||
               attempt == retries) {
      stop("HTTP ", r$status_code, ": ", substr(rawToChar(r$content), 1, 300),
           call. = FALSE)
    }
    Sys.sleep(min(60, 2^attempt))
  }
}

parse_chat_response <- function(res) {
  ch <- res$choices[[1]]
  content <- ch$message$content %||% ""
  lp <- ch$logprobs$content
  first <- NULL
  if (length(lp)) {
    top <- lp[[1]]$top_logprobs
    if (length(top)) {
      first <- data.frame(
        token = vapply(top, function(t) t$token %||% "", ""),
        logprob = vapply(top, function(t) as.numeric(t$logprob %||% NA), 0),
        stringsAsFactors = FALSE
      )
    } else {
      first <- data.frame(token = lp[[1]]$token %||% "",
                          logprob = as.numeric(lp[[1]]$logprob %||% NA),
                          stringsAsFactors = FALSE)
    }
  }
  list(content = content, logprobs = first)
}
