#import "@preview/one-liner:0.2.0": fit-to-width

#let lbar = block(spacing: 0.62em, line(length: 100%))

#let make-title(name, perm_addr, school_addr, phone, email, linkedin, github) = [
  #set block(spacing: 0.5em)
  #fit-to-width[#text(size: 16pt)[#strong[#name]]#h(1fr)
  #phone #h(1fr)|#h(1fr) #email #h(1fr)|#h(1fr) #linkedin #h(1fr)|#h(1fr) #github]
  #fit-to-width[Mailing Address: #perm_addr | School Address: #school_addr]
]

#let entry(
  title: "Entry title not defined",
  subtitle: none,
  city: none,
  state: none,
  start-date: none,
  end-date: none,
  tags: none,
  ..works) = {
  let two-line-title = subtitle != none and (city != none or state != none)

  if title != "_no_title" { strong[#title. ] }

  if two-line-title { h(1fr) }
  if city != none and state != none {
    emph[#city, #state]
    text[.]
  }

  if two-line-title { linebreak() }
  if subtitle != none {
    emph[#subtitle]
  }

  if start-date != none {
    h(1fr)
    text[#start-date]
    if end-date != none { text[ \- #end-date] }
  }

  list(..works)
}
// block[#strong[#title.] #emph[#city, #state] \ #body]

#let section(title: "Section title not undefined", 
  ..entries) = {
  text(size: 12pt, strong[#title \ ])
  for entry in entries.pos() {
    [
      #entry
    ]
  }
}

#let resume(header: none, ..sections) = {
  if header != none { header } else { text(red, size: 32pt)[Header not defined.] }
  for sec in sections.pos() {
    lbar
    sec
  }
}
