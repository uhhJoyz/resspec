#import "./styles.typ": *
#import "@preview/one-liner:0.2.0": fit-to-width

#set page(margin: 0.8in)
#let bullet(title, body) = [#title: #body]
#let subtitle(title, start, end) = [#h(1em)#emph[#title], #start - #end]

#set text(font: "Noto Sans", size: 10pt)
#show math.equation: set text(font: "Noto Sans Math")
#show "Pokemon" : "Pokémon"
#show link: it => text(fill: color.hsl(268deg, 65%, 65%), it)

#let med-spc = 0.6em
#let sm-spc = 0.4em
#set block(spacing: med-spc)
#set list(spacing: sm-spc)
#set par(spacing: sm-spc)
#show list: set par(leading: sm-spc)

#resume(
  header: make-title[
      // name
      Diglett
    ][
      // permanent address
      18 Diglett Cave, Kanto Region
    ][
      // school address
      54 Vermilion City, Kanto Region
    ][
      // phone
      #link("tel:123-456-7899")
    ][
      //email
      #link("mailto:diglett@diglett.com")
    ][
      #link("https://www.linkedin.com/in/diglett-not-a-real-user")[LinkedIn]
    ][
      #link("https://www.github.com/uhhJoyz")[GitHub/uhhjoyz]
    ],
  section(title: "Education", 
    entry(
      title: "Vermilion Gym",
      subtitle: [Master of Electric Types, 4.0/4.0 GPA],
      city: "Vermilion City",
      state: "Kanto",
      start-date: [May 2025],
      end-date: [May 2026],
      [Relevant Courses: Electric Type Mastery, Superb Digging, etc.],
      [Graduate Research Advisor: Lt. Surge]
    ),
    entry(
      title: "_no_title",
      subtitle: "Bachelor of Digging, 3.9/4.0 GPA",
      start-date: [Aug. 2021],
      end-date: [May 2025],
      [Undergraduate Research Advisor: Lt. Surge]
    ),
  ),
  section(
    title: "Experience",
    entry(
      tags: [rock slide,rock type],
      title: "Presentations and Publications",
      start-date: [Sep. 2025],
      end-date: [Dec. 2025],
      [Presented _The Art of Rock Slide_ at ElectriCon 2025],
      [Published _The Art of Rock Slide_ in ECP'25],
    ),
    entry(
      tags: [],
      title: "Ground Type Researcher",
      city: "Vermilion City",
      state: "Kanto",
      start-date: "Sep. 2024",
      end-date: "present",
      [Achieved magnitude 10 on three consecutive attacks],
      [Developed and implemented novel high-magnitude booster item],
    ),
    entry(
      tags: [],
      title: "Teaching Assistant at Vermilion City Gym",
      city: "Vermilion City",
      state: "Kanto",
      [TA for Digging Development, Rock-ing Proficiency, and Magnitude Mastery],
      [Met weekly with fellow Pokemon to provide specific actionable advice for their move learning studies],
      [Managed a team of 3 Pokemon to grade nearly 200 assignments weekly],
      [Built relationships with and helped Pokemon prepare for exams with several regularly returning Pokemon],
    ),
    entry(
      tags: [],
      title: "Undergraduate Digging Intern",
      city: "Vermilion Mining Co.",
      state: "Kanto",
      start-date: "Feb. 2023",
      end-date: "Aug. 2024",
      [Participated in summer Computing for Global Challenges (C4GC) internship program],
      [Planned and implemented 3 Python-based integer linear programming solvers with Dr. S. S. Ravi for NP-hard problems such as the Minimum Set Cover and Minimum Disjoint Descriptor using Gurobi],
      [Theorized seven heuristic approximation algorithms for NP-hard problems, allowed for polynomial time approximation at the cost of some accuracy],
      [Implemented heuristic solvers for NP-hard problems in Rust, benchmarking over $10 times$ average speedup over Python-based solver],
    ),
  ),
  section(
    title: "Course Projects & Specializations",
    entry(
      tags: [],
      title: "Fast and Efficient Digging - Independent Study",
      start-date: "Jun. 2025",
      end-date: "present",
      [Under Lt. Surge, studied the art of quickly and efficiently digging in a group of 2 peers],
      [Covered topics including dig latency, pipelined digging, coherent entry and exit holes, maintaining tunnel states, and optimal dig timing],
      [Created a digging guide which enhances dig speed by nearly $716%$ for untrained ground types and $495%$ for all other types],
    ),
    entry(
      tags: [],
      title: [_Hulk Smash_: Fast and Scalable Rock Smash Dodging],
      subtitle: [Rock Type Safety],
      start-date: "Fall 2025",
      [Observed hundreds of fighting type attacks to extract optimal dodging patterns for opponents using rock smash],
      [Provides $15%$ increased evasion for rock types under $12$kg and $5%$ evasion for rock types greater than or equal to $12$kg],
      [Enabled $150$ out of $490$ evaluated rock types to avoid fainting when facing perilous fighting type matchups],
    ),
    entry(
      tags: [],
      title: "The Flying Tango: Undergraduate Capstone",
      start-date: "Fall 2024",
      end-date: "Spring 2025",
      [Under the guidance of Lt. Surge, created a machine which successfully teaches one in three digletts to fly, allowing more than $168$ digletts to escape capture by Team Rocket],
      [Used the teach and teach method to impress the importance of flying maneuvers upon young, impressionable digletts],
    ),
    entry(
      tags: [],
      title: "Magnitude 11",
      subtitle: "Advanced Dig Development",
      start-date: "Fall 2023",
      [Served as Dig Master on a team of 5 ground type engineers to develop the _Magnitude 11_ teaching application, which allows the user to reach magnitude 11],
      [Performed requirements elicitation and guided beta testing with 17 Vermilion Gym students, forming qualitative and quantitative assessments of user needs and product function],
      [Conducted rigorous security testing, identifying and patching 3 security vulnerabilities which allowed attackers to limit viewers to magnitude 3],
    ),
    entry(
      tags: [],
      title: "Relevant Course Projects",
      subtitle: "Ground Type Systems/Rockware Engineering",
      [Created virtual groundspace addressing and access system and diglett-level multitasking system for troops of digletts],
      [Implemented simulation of pipelined TM viewing with 5 stages of teaching],
      [Produced a 17-page paper regarding comparisons of rock type classification models and initial findings for optimal rock smash avoidance policies],
    ),
  ),
  section(
    title: "Passion Projects",
    entry(
      tags: [],
      title: "Mud Bathing",
      start-date: "May 2025",
      end-date: "present",
      [Bathed in mud for nearly 18 hours per day, 2 days per week over the course of nearly a year],
      [Convinced nearly 9,000 other Pokemon to bathe in mud, allowing electric types in 5 cities to safely nuzzle their trainers],
    ),
    entry(
      tags: [],
      title: "Used Resspec",
      start-date: "Dec. 2025",
      [Used ResSpec, an automated resume specializer application written in Ocaml which automatically selects the most relevant portions of a user's work experience and outputs a PDF resume, rendered from Typst code],
      [_Assembled this resume_ using ResSpec, implemented using a reduction to the knapsack problem which matches tags of a job to resume experiences based on user-selected skill point investments],
    ),
    entry(
      tags: [],
      title: "Rolled Rocks",
      start-date: "May 2025",
      [Rolled nearly 250 rocks over 5 miles to their delivery destinations for fun],
      [Accidentally pushed 3 dwebbles away from home and delivered them back safely using a sophisticated network of more than 79 tunnels between towns],
      [Created a rock rolling system which allows one diglett to roll more than 10 rocks concurrently],
    ),
    entry(
      tags: [],
      title: "The Dynamic Diglett Duo",
      start-date: "May 2024",
      [Performed as part of the _Dynamic Diglett Duo_ band in 15 concerts in the month of May, celebrating the lesser-known Ground Type festival of Lavender Town],
      [Sterilized more than 5,000 chairs after stinky electric types used them to enjoy the festival],
    ),
    entry(
      tags: [],
      title: "Moo Moo Milking",
      start-date: "Mar. 2023",
      [Helped farmers milk more than 10,000 Miltanks to feed hungry digletts and dugtrios],
      [Provided warm Moo Moo Milk to more than 150,000 Pokemon],
    ),
    entry(
      tags: [],
      title: "Styling my Hair",
      start-date: "Mar. 2023",
      [Styled my hair (all three strands) every day for more than 10 years, serving as a role model for aimless young digletts everywhere.],
    ),
    entry(
      tags: [],
      title: "Speed Reading",
      start-date: "Feb. 2023",
      [Read a lot of things really really really fast!],
    ),
  ),
  section(
    title: "Skills",
    entry(
      title: "_no_title",
      [Digging and rock sliding],
      [Singing],
    ),
  ),
)
