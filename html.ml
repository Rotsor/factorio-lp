open Core_kernel
    
type t = string

let concat ts = String.concat ~sep:"" ts
  
let tag tag body =
  String.iter tag ~f:(function
      | 'a'..'z' -> ()
      | _ -> raise_s [%sexp "tags should be made of lowercase latin letters", (tag : string)]);
  sprintf "<%s>%s</%s>" tag body tag

let tag_multi s body = tag s (concat body)

let table = tag_multi "table"
let tr = tag_multi "tr"
let td = tag "td"
let th = tag "th"

let html_escape s =
  String.concat_map s ~f:(function
      | '&' -> "&amp;"
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '"' -> "&quot";
      | '\'' -> "&#39";
      | c -> String.make 1 c)

let text s = html_escape s

let link ~url t =
  sprintf "<a href=\"%s\">%s</a>" (html_escape url) t

let anchor ~id t =
  sprintf "<a id=\"%s\">%s</a>" (html_escape id) t


let style = {css|
  <style type="text/css">

  </style>
  |css}

let render x =
  sprintf {|<html>
            <link href="style.css" rel="stylesheet" type="text/css"/>
            <body>%s</body></html>|}
    (style ^ x)

let div ?class_ t =
  match class_ with
  | None ->
    tag "div" t
  | Some class_ ->
    sprintf {|<div class="%s">%s</div>|} (html_escape class_) t

let img ~url = sprintf {|<img src="%s" height="32" width="32"></img>|} url
