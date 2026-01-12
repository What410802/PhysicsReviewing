#import "@preview/unify:0.7.1": unit, qty
#import "@preview/showybox:2.0.4" as Showy
#import "@preview/frame-it:2.0.0" as Frame
// #import "@preview/splash:0.5.0"
#import "@preview/pigmentpedia:0.3.3" as P
#import "@preview/fancy-tiling:1.0.0" as Tiling
#import "@preview/physica:0.9.7" as Ph: dd
#import "@preview/cetz:0.4.2"
#import "@preview/cetz-plot:0.1.3"
// #import "@preview/equate:0.3.2"

#set text(fallback: false)
#set text(lang: "en", font: (
  // "New Computer Modern",
  "Noto Serif",
  "Noto Emoji",
))
// #show math.equation: set text(font: "Noto Sans Math")
#let change-style = (
  zh: it => {
    let font = (
      "Noto Serif CJK SC",
      "Noto Emoji",
    )
    set text(font: font)
    // show math.equation: set text(font: font)
    // set math.equation(block: false)
    set par(first-line-indent: (amount: 2em, all: true))
    it
  },
)
#let set-lang(lang, it) = {
  set text(lang: lang)
  if lang in change-style.keys() {
    show: change-style.at(lang)
    it
  }
  else {
    it
  }
}

#set par(justify: true)
#show figure.where(kind: table): set figure.caption(position: top)
#show math.equation.where(block: false): math.display
#show math.equation: set block(breakable: true)
#show sym.gt.eq: sym.gt.eq.slant
#show sym.lt.eq: sym.lt.eq.slant
// #show sym.parallel: sym.slash.double

#show heading.where(level: 3): align.with(center)

#let bar_author = text(font: "Libre Barcode 128",)[By bishaokang24\@stu.XJTU.edu.cn]
#let bar_byheart = text(font: "Libre Barcode 128 Text",)[Made with \<3 \~]
#let header = {
  set text(size: 10pt)
  box()[_University Physics_ Review Note]+h(1fr)+box(bar_byheart)+h(1fr)+box(bar_author)
}
#set page(numbering: "1", margin: (x: 15mm, y: 20mm), header: header)
#set page(columns: 2)

// #set Showy.showybox(breakable: true)
#let myShowyDefaultParam = (breakable: true)

#let myMathRect(body) = rect(stroke: .5pt, inset: .25em, math.equation(block: true, numbering: none, body))
#let myEquationNote(it, buffer: h(1em), wrap-f: it=>$(it)$, newline: false, ..text-params) = {
  set text(size: 8pt, fill: gray.darken(40%), ..text-params)
  if newline {$\ $}
  buffer
  wrap-f(it)
}

#let hl_color = rgb("#fffd11a1")
#let Typs_color = (
  length: rgb("#FFEDBF"), // Also: int, float, decimal, relative, fraction
  label: rgb("#C6D6ED"),
)
#let myRoundBox(..opt, body) = box(fill: Typs_color.length, radius: 1em/4, outset: (y: .3em), inset: (x: .3em), ..opt, body)
#let myDimLabel = {
  set text(size: .6em)
  set text(font: "Noto Sans Mono")
  box(move(myRoundBox("Dimension", fill: Typs_color.label), dy: -.6em))
}
#let myDimBare(s, ..opt) = {
  // assert(type(s) == str)
  if type(s) == str {
    s = unit(s, ..if s.matches("/").len() < 2 and s.match("1/") == none {(per: "\/")} else {()})
  }
  // set text(top-edge: "ascender", bottom-edge: "descender")
  myRoundBox(s, stroke: 1em/24)
}
#let myDim(s, ..opt) = {
  myDimBare(s, ..opt)
  // sym.wj // Surprising: This is needless but the one below is a must.
  h(.25em)
  sym.wj
  // (box(rotate(90deg, origin: center+horizon, reflow: true, text(size: .6em, `-DIM-`))))
  myDimLabel
}
#let myDef(..opt, body) = {
  let _f = strong
  _f($frak("Def.")|$)
  " "
  body
  _f($||$)
  //+emoji.playback.stop
}

#title()[2025-2026-1-2]

#outline()

= Thermodynamics

#let Thermodynamics-table = {
set table(fill: (x, y) => 
  if y==0 {P.zhongguo.zh.灰白.铅白}
  else if x==0 and y!=0 {}
)
show table.cell: align.with(center+horizon)
show table.cell.where(x: 0): it => {
  set text(weight: "bold")
  
  set align(horizon+center)
  rotate(-90deg, reflow: true, it)
}
show table.cell.where(x: 2): set text(style: "italic")
let double-line = table.cell(colspan: 5, inset: 1/12*1em)[]
figure(
  caption: [Symbols That Are Unfamiliar],
  table(
    columns: (auto, ) * 5,
    
    table.header(none, [*Symbol*], [*Viewpoint#numbering("*", 1)\ #text(size: .7em)[(#underline[M]acro | #underline[m]icro)]*], [*Dimension#numbering("*", 2)\ #text(size: .7em)[(in units)]*], [*Explanation*]),
    
    table.cell(rowspan: 5, text(fill: P.crayola.standard.blue-violet)[Statics]),
      $nu$, [M], myDimBare("mol"), [],
      $N$, [m], myDimBare($1$), [Number of molecules],
      $n$, [C (m)], myDimBare("1/m^3"), [],
      [$m_0$ ($mu$)], [m], myDimBare("kg"), [Mass of a molecule],
      $M$, [C (M)], myDimBare("kg/mol"), [],

    double-line,
    table.cell(rowspan: 3, fill: P.crayola.standard.yellow, )[_Kinematics_],
      $overline(epsilon)$, [m], myDimBare("J"), [Average molecular kinetic energy; $overline(epsilon) = 1/2 mu overline(v^2)$],
      $f(v)$, [C], myDimBare("m^-1 s"), [Percentage of molecules near $v$, i.e. $f(v) dd(v) = dd(N)/N$],
      $i$, [(m)], myDimBare($1$), [Degree of freedom (DOF). $i = 3+{0,2,3}$],

    double-line,
    table.cell(rowspan: 4, fill: Tiling.honeycomb(cell-stroke: P.zhongguo.zh.黄.鹅黄, background-color: P.zhongguo.zh.黄.琥珀), highlight(fill: P.zhongguo.zh.黄.琥珀, text(fill: white)[Dynamics])),
      $C_(p,m)$, table.cell(rowspan: 4)[C (M)], table.cell(rowspan: 2, myDimBare("J/mol/K")), table.cell(rowspan: 2)[$ C_x = /*lr((Delta Q)/(Delta T) mid(bar))_x*/ ((Delta Q)/(Delta T))_x $],
      $C_(V,m)$,
      $c_x$, myDimBare("J/kg/K"), [],
      $gamma$, myDimBare($1$), [$gamma = C_(p,m)\/C_(V,m)$],

    double-line,
    table.cell(rowspan: 4)[Constants],
      [$k$ ($k_B$)], none, myDimBare("J/K"), [#underline[B]oltzmann constant. $k approx qty("1.381e-23", "J/K", per: "\/")$ (`k_B`)],
      $R$, none, myDimBare("J/mol/K"), [Ideal gas constant. $R approx qty("8.314", "J/mol/K")$ (`IdealGas`)],
      $N_A$, none, myDimBare("mol^-1"), [$N_A = qty("6.02214076e23", "mol^-1")$],
      $V_m$, [(M)], myDimBare("m^3/mol"), [Molar volume of ideal gas at #qty("273.15", "K"), #qty("101.325", "kPa"). $V_m approx qty("22.4", "m^3/mol", per: "\/")$ (`V_m101`)]
  )+align(left)[
    #set enum(numbering: "*")
    + The column "Viewpoint" refers to the scale of measurement on molecules. "C" (#underline[C]onstant) means this is independent on our viewpoint. /*The main criterion for judging "M" or "m" is whether the dimension of the quantity contains #unit("mol") or #unit("mol^-1").*/ "m" usually means this quantity focuses on the behavior of a single molecule, and when we use macro/micro view to measure a quantity that is actually micro/macro, we will usually get a overly large/small value (in the same unit system).

    + In following contents, dimensions are usually expressed in units (SI) for convenience.
  ]
)
}
#place(scope: "parent", float: true, bottom, clearance: 1em, Thermodynamics-table)

// #columns(2)[
== Formula Essential

- Relationships between constants: $ R = k_B N_A. $

- Constants for specific ideal gases: $
    C_(V,m) &= i/2 R,\
    C_(p,m) &= (1+i/2)R,\
    C_(p,m) &= C_(V,m) + R #[(Mayer formula)],\
    gamma &= 1+2/i;
  $ Related quantities: $
    E_"per-DOF" &= 1/2 k T, #myEquationNote[For a single molecule]\
    => U &= i/2 nu R T,
  $
  where $i = t+r+2s$, because each $s$ leads to 2 shares of both kinetic energy and potential energy.

- On $p$ and $T$: $
    p V &= nu R T,\
    p &= n k T,\
    p/rho &= (R T)/M.
  $

- On other $p$: $
    p &= 1/3 rho overline(v^2)\
      &= 2/3 n overline(epsilon).
  $

#let frame-blue = P.zhongguo.zh.蓝.群青
#let myShowyFrameBlue = (
  border-color: frame-blue,
  title-color: frame-blue,
  body-color: frame-blue.lighten(90%),
  footer-color: frame-blue.lighten(60%)
)
- #Showy.showybox(
  title: [*Maxwell's law of speed distribution*],
  footer: [(This won't be tested in exams. #emoji.face.smile)],
  frame: myShowyFrameBlue,
  ..myShowyDefaultParam
)[$
  f(v) &= 4pi(mu/(2pi k T))^(3/2)v^2 "e"^(-(mu v^2)/(2 k T))\
    &= 4/sqrt(pi) dot 1/v (1/k dot epsilon/T)^(3/2) exp(- 1/k dot epsilon/T)
$]

- 3 "$v$": $
    v_p &= sqrt((2 R T)/M) #[(Most probable $v$)],\
    sqrt(overline(v^2)) &= sqrt((3 R T)/M),\
    overline(v) &= sqrt(8/pi (R T)/(M)).
  $

- About distributions... $
    n/n_0 = p/p_0 = "e"^(- epsilon_p/(k T)),
  $ where $epsilon_p = m_0 g h$ is the potential energy of a molecule. *#highlight[Note:]* This also applies to conditions where $epsilon$ is discrete! #emoji.face.wink

- Molecular kinetics:\
  Mean-free-path length $
    overline(lambda) = 1/(sqrt(2)pi d^2 n),
  $ where $d$ is the effective diameter;\
  Mean collision frequency $
    overline(Z) = overline(v)/overline(lambda) = sqrt(2)pi d^2 n overline(v).
  $

== Macro view begins...

- First law of thermodynamics: $
    // "đ"Q = dd(E) + "đ"W,
    Q = Delta E + W,
  $
  where $Q$ is the heat from the external to the gas; $E$ is the internal energy of the gas; $W$ is the work made by the gas to the external.

  #Showy.showybox(
    title: [*Some derivation around it...*],
    frame: myShowyFrameBlue,
    ..myShowyDefaultParam
  )[
    $ C_(V,m) = dd(E)/dd(T), $
    $ (dd(E)/dd(T))_p = (dd(E)/dd(T))_V = dd(E)/dd(T), $
    $
      => C_(p,m) &= dd(E)/dd(T) + p(dd(V)/dd(T))_p\
      &= C_(V,m)+R.
    $
  ]

  #Showy.showybox(
    title: [*About #underline[Adiabatic process]...*],
    frame: myShowyFrameBlue,
    ..myShowyDefaultParam
  )[
    $ "d"(p V) = nu R dd(T), $
    #strike[Breaking it down to 2 aspects] We can cancel out $dd(T)$ using
    $ p dd(V) = "đ"W = - dd(U) = - nu C_(V,m) dd(T), $
    and get
    $ (C_(V,m) + R)p dd(V) + C_(V,m)V dd(p) = 0. $
    That is,
    $ myMathRect(dd(p)/p + gamma dd(V)/V = 0\,) $
    // or $ dd(p)/dd(V) = - gamma p/V $
    or $ myMathRect(p V^gamma = C.) $
    We can also use $p V T^(-1) = nu R$ to get relationships between $p$, $V$ and $T$, i.e.:
    $
      V^(gamma-1) T &= C',\
      p^(1-gamma) T^gamma &= C''\
      (#[or] p^(gamma-1) T^(-gamma) &= C''').
    $
    The exponents of $p,V,T$ follow
    $
      &([p], [V], [T])\
      =& alpha (1,gamma,0) + beta (1,1,-1)\
      =& (alpha+beta, alpha gamma + beta, -beta) #h(.5em) (alpha, beta in RR)\
      =& beta (1 + alpha/beta, 1 + gamma alpha/beta, -1).
    $

    #line(length: 100%, stroke: .5pt)
    
    #text(size: 8pt)[Based on the basic knowledge above, we can talk about more high-level things...]

    In adiabatic process, work made by the gas
    $
      W &= integral_(V_1)^(V_2) p dd(V) #myEquationNote[Area under $p$--$V$ curve]\
      &= integral_(V_1)^(V_2) (p_1 V_1^gamma)/(V^gamma) dd(V) #myEquationNote[Using initial condition]\
      &= (-Delta (p V))/(gamma-1) = - (nu R)/(gamma-1)Delta T\
      &= - nu C_(V,m) Delta T #myEquationNote[Comply with previous]
    $
  ]
  
- About $eta$...
  $ eta = W/Q_"Absorb" = 1 - Q_"Emit"/Q_"Absorb", $
  where $Q_"Absorb"$ is usually denoted by $Q_1$, and $Q_"Emit"$ is by $Q_2$.
  
  Refrigeration coefficient $
    w = Q_2/W,
  $
  where $Q_2$ is the heat absorbed from the #underline[_designated_] cold source by the working medium.

  - Carnot heat engine (Clockwise)
    
    1. Constant $T$
    
      $ T = T_H, V_1 arrow.tr V_2 $
      $ Q_H = nu R T_H Delta ln(V) #myEquationNote[External #sym.arrow gas] $
    
    2. Adiabatic
    
      $
        V^(gamma-1) T = V_2^(gamma-1) T_H, T_H arrow.br T_L,\
        V_2 arrow.tr V_3, Delta U arrow.br= i/2 nu R Delta T
      $

    3. Constant $T$
      
      $T = T_L$. Similarly, we can get $Q_L$.

    4. Adiabatic

      Similar.

    From the adiabatic processes, we can get $V_2/V_1 = V_3/V_4$ or $V_1/V_4 = V_2/V_3$.
    
    Finally, we can get the efficiency $ eta &= 1 - Q_L/Q_H\
      &= 1 - (nu R T_L ln V_3/V_4)/(nu R T_H ln V_2/V_1)\
      &= 1 - T_L/T_H
    $

    #let carnot_principle = cetz.canvas({
      import cetz.draw as draw
      import cetz-plot.plot as P
    
      let my_params = (
        constants: (
          // N_A: 6.02214076e23,
          V_m: 22.4e-3,
          R: 8.314e-3, // ..e0
        ),
        working_medium: (
          gamma: 5.4, // 1.4
          V_3: 2e-0, // ..e-3
          V_3_on_V_4: 1.5,
        ),
        externals: (
          T_L: 300,
          T_H: 600,
        )
      )
      let nu_R = my_params.working_medium.V_3 / my_params.constants.V_m * my_params.constants.R
      let C_H = nu_R * my_params.externals.T_H
      let C_L = nu_R * my_params.externals.T_L
      let C_4 = C_L * calc.pow(my_params.working_medium.V_3 / my_params.working_medium.V_3_on_V_4, my_params.working_medium.gamma - 1)
      let C_2 = C_L * calc.pow(my_params.working_medium.V_3, my_params.working_medium.gamma - 1)
    
      let my_style = (
        line: (
          cir: (stroke: (thickness: 1.5pt)),
          outer: (stroke: (dash: "densely-dashed", thickness: 1.5pt)),
          axis_perpendicular: (stroke: (dash: "dashed", thickness: .75pt)),
        )
      )
      let point_sym = ($a$, $b$, $c$, $d$)
      let point_sym_anchors = ("base-west", "west", "west", "west")
    
      let p--V__T_H = V => C_H/V
      let p--V__T_L = V => C_L/V
      let p--V__2 = V => C_2/calc.pow(V,my_params.working_medium.gamma)
      let p--V__4 = V => C_4/calc.pow(V,my_params.working_medium.gamma)
      let V_values = for a in (C_4/C_H, C_2/C_H, C_2/C_L, C_4/C_L) {(calc.pow(a, 1/(my_params.working_medium.gamma - 1)),)}
      let p_values = for (V, C_HL) in V_values.zip((C_H, C_H, C_L, C_L)) {(C_HL/V,)}
    
      let V_max = V_values.at(3-1)*1.2
      let p_max = p_values.at(1-1)*1.2
      let eps = 1e-9 /*9.999e-299*/
    
      draw.set-style(
        shared-zero: false,
      )
      P.plot( // TODO: Add arrows to the 4 edges
        size: (6, 6),
        axis-style: "school-book",
        x-min: 0, x-max: V_max,
        y-min: 0, y-max: p_max,
        x-label: $V$, y-label: $p$,
        x-tick-step: none, y-tick-step: none, // Off when DEBUG
        x-ticks: for i in range(4) {((V_values.at(i), $V_#(1+i)$),)},
        y-ticks: for i in range(4) {((p_values.at(i), $p_#(1+i)$),)},
        {
          P.add-fill-between(
            V => {
              if V < V_values.at(2-1) {p--V__T_H} else {p--V__2}(V)
            },
            V => {
              if V < V_values.at(4-1) {p--V__4} else {p--V__T_L}(V)
            },
            domain: (V_values.at(1-1), V_values.at(3-1)),
            style: (fill: gray)
          )
          P.add(
            p--V__T_H,
            domain: (eps, V_max),
            style: my_style.line.outer,
          )
          // (solid part)
          P.add(
            p--V__T_H,
            domain: (V_values.at(1-1), V_values.at(2-1)),
            style: my_style.line.cir,
          )
    
          P.add(
            p--V__T_L,
            domain: (eps, V_max),
            style: my_style.line.outer,
          )
          // (solid part)
          P.add(
            p--V__T_L,
            domain: (V_values.at(3-1), V_values.at(4-1)),
            style: my_style.line.cir,
          )
          // Adiabatic-2
          P.add(
            p--V__2,
            domain: (V_values.at(2-1), V_values.at(3-1)),
            style: my_style.line.cir,
          )
          // Adiabatic-4
          P.add(
            p--V__4,
            domain: (V_values.at(4-1), V_values.at(1-1)),
            style: my_style.line.cir,
          )
    
          P.annotate({
            for i in range(4) {
              let _P = (V_values.at(i), p_values.at(i))
              draw.content(_P, point_sym.at(i), anchor: point_sym_anchors.at(i))
              for onto in (((0,0), (0,1)), ((0,0), (1,0))) {
                draw.line(_P, (project: _P, onto: onto), ..my_style.line.axis_perpendicular)
              }
            }
            
            draw.content((V_max * 1.05, p--V__T_H(V_max)), $T_H$)
            draw.content((V_max * 1.05, p--V__T_L(V_max)), $T_L$)
          })
    
          let marks = (
            be => (end: ")>", fill: black, length: (be.at(0).at(1)-be.at(1).at(1)) * .3, width: (be.at(0).at(0)-be.at(1).at(0)) * .15), // !
            be => (end: ")>", fill: black, length: (be.at(0).at(1)-be.at(1).at(1)) * .33, width: (be.at(0).at(0)-be.at(1).at(0)) * .24), // !
          )
          
          // Q_H curly arrow
          P.annotate({
            let mid_V = (V_values.at(0) + V_values.at(1)) / 2
            let mid_p = p--V__T_H(mid_V)
            let be = (
              (mid_V * 1.1, mid_p * 1.1),
              (mid_V * .95, mid_p * .92),
            )
            draw.bezier(
              ..be,
              (mid_V * .96, mid_p * 1.04), // this is control
              mark: marks.at(0)(be),
            )
            draw.content((mid_V, be.at(0).at(1)), $Q_H$, anchor: "west")
          })
          
          // Q_L's
          P.annotate({
            let mid_V = V_values.at(2) * .3 + V_values.at(3) * (1 - .3)
            let mid_p = p--V__T_L(mid_V)
            let be = (
              (mid_V * 1.05, mid_p * 1.2),
              (mid_V * .95, mid_p * .85),
            )
            draw.bezier(
              ..be,
              (mid_V * .96, mid_p * 1.08), // this is control
              mark: marks.at(1)(be),
            )
            draw.content((mid_V, be.at(1).at(1)), $Q_L$, anchor: "east")
          })
        }
      )
    })

    #figure(
      caption: [Carnot heat engine principle],
      carnot_principle
    )

== Things to Recite (Maybe)...

=== 1st Law of Thermodynamics

$ "đ"Q = dd(E) + "đ"W $

#set-lang("zh")[
  系统从外界吸收的热量，一部分使其内能增加，另一部分则用以对外界做功.
]

=== 2nd Law of Thermodynamics

#set-lang("zh")[
  / 开尔文表述（Kelvin Statement）: 不可能从单一热源吸收热量，使之完全转化为功而不引起其他变化.
  / 克劳修斯表述（Clausius Statement）: 不可能使热量从低温物体传向高温物体而不引起其他变化.
]

=== Carnot's theorem

// Two contents:

// (1) All reversible heat engines operating between two given heat sources at temperatures $T_1$ and $T_2$ have the same efficiency, which is equal to the efficiency of an ideal gas reversible Carnot heat engine, i.e., $η=1-T_2\/T_1$.

// (2) All irreversible heat engines operating between the same high and low temperature heat sources cannot have an efficiency greater than that of reversible heat engines
// Carnot's theorem points out the direction to improve the efficiency of heat engines.

#set-lang("zh")[
  #set enum(numbering: "(1)")
  卡诺定理的内容有两条：
  
  + 在温度分别为 $T_1$ 与 $T_2$ 的两个给定热源之间工作的一切可逆热机，其效率相同，都等于理想气体可逆卡诺热机的效率，即 $eta = 1 - T_2\/T_1$.
  
  + 在相同的高、低温热源之间工作的一切不可逆热机，其效率都不可能大于可逆热机的效率. 
]

// #colbreak()

== The Entropy World

#let quote = (..it) => Frame.frame-style(Frame.styles.hint)(Frame.frame("Quote", luma(80%))(..it))
// This syntax works as well, but colors are not generated automatically
// #let syntax = Frame.frame("Syntax", green)
// This is necessary. Don't forget this!
Some conditions for the definition of $S$:

#quote[Single-valued][Similar to $U$, $S$ should be a function of the (micro) state of the system.]
#quote[Additive][$S$ of a system should be the _sum_ of $S$ of each part of the system. (However, $Omega$ of a system is the _product_ of each part of it.)]

$ S = k ln Omega, #myEquationNote(newline: true, size: 10pt)[*Bolzmann's principle*] $

$ dd(S) >= 0.\ #myEquationNote[Principle of entropy increase] $

All processes of an isolated system are processes of increasing $S$, and $S$ reaches its maximum when the system are at an equilibrium state.

In a free expansion from $V_1$ to $V_2$, 
$ Omega_2/Omega_1 = (V_2/V_1)^N, $
where $N$ is the molecule number of the gas. Thus,
$ Delta S = k N ln V_2/V_1 = nu R Delta ln V. $

So, in this kind of process, $dd(S) <= 0 => V_2/V_1 >= 1.$

#[
  #line()
  #set text(size: 10pt)
  Below are some knowledge points in "\*":
  
  - For an infinitely small reversible isothermal process, $dd(S) = ("đ"Q)/T. $
]

#pagebreak()

= Quantum Mechanics

/ Planck constant: $ h = qty("6.62607015e-34", "kg m^2 s^-1") "or" unit("J s") $

== Intro: 4 phenomena...

=== Thermal Radiation

When radiation and absorption reach equilibrium, $T$ of the object no longer changes and the object enters the state of #underline[thermal equilibrium].

Monochromatic radiant exitance $ M_lambda (T) = dd(M_lambda)/dd(lambda), $ where $dd(M_lambda)$ #myDim("W/m^3") is the radiant power in $[lambda, lambda+dd(lambda)]$.

Radiant exitance $ M(T) #myDim("W/m^2") = integral_0^infinity M_lambda (T) dd(lambda). $

#set-lang("zh")[
  能够全部吸收各种波长辐射能而完全不发生反射和透射的物体称为*绝对黑体*或*黑体*.
]

/ Stefan-Boltzmann's Law: $ M_B (T) = sigma T^4, $ where $#sym.zws _B$ denote #underline[b]lackbody, and $sigma approx #qty("5.670e-8", "W m^-2 K^-4")$.
/ Wien's displacement law: $ lambda_"m" = b/T, $ where $lambda_"m"$ is the $lambda$ with the max magnitude, and $b = #qty("2.898e-3", "m K")$.
/ Planck's formula: $ M_(B lambda)(T) = (2 pi h c^2 lambda^(-5))/("e"^((h c)/(lambda k T)) - 1), $ where $h = #qty("6.626e-34", "J s")$ (Planck's constant).

=== Photoemission

Phenomena:

1. Saturation current
// Not sure whether TODO
2. Cut-off frequency
3. stopping voltage
4. Photoelectrons are emitted in real time, with a lag time of no more than #qty("1e-9", "s").

=== Compton Effect

When photons interact with particles, the effects depends on the energy of photons. From low to high is: 1. photoemission ($h nu < #qty("0.5", "MeV")$); 2. Compton effect; 3. generating electron-positron pairs (#sym.gamma photons, $h nu > #qty("1.02", "MeV")$).

(In contrast, _Rayleigh scattering_ is the scattering with a constant wavelength.)

In Compton effect, there are some hypotheses:

#quote[Electrons are _free_][
  X-ray photons mainly interact with the electrons that are weakly bound to the nuclei in the scattering material. So these electrons are deemed as free electrons.
]
#quote[Electrons are _static_][
  Because the energy of X-photons are much higher than thermal motion energy of these electrons, so the latter are approximately zero.
]
#quote[About the process...][
  - _Relativistic effects_ cannot be ignored.
  - The collision is _completely elastic_.
]

Based on these, we have
$
  (h nu)/c mat(cos theta; sin theta) + m v mat(cos(-phi); sin(-phi)) = (h nu_0)/c mat(1; 0), #myEquationNote[Momentum]\
  h nu_0 + m_0 c^2 = h nu + m c^2, #myEquationNote[Energy]
$
together with
$ m = m_0/sqrt(1 - (v/c)^2), lambda = c/nu. $

Solving them, we have $ myMathRect(Delta lambda = c/(nu parallel -nu_0) &= lambda_C (1 - cos theta)\ &= 2 lambda_C sin^2 theta/2\,) $ where $lambda_C = h/(m_0 c)$ is the Compton wavelength of electron.

=== The spectrum of Hydrogen

Assumptions of Bohr's theory:

#quote[Stationary state][
  Atoms can only exist in a series of stable states, with discontinuous energy.
]
#quote[Photons...][
  $ nu_(k -> n) = abs(E_k - E_n)/h. $
]
#quote[Orbital angular momentum $L = m v r$ is _Quantized_][
  $ L = n planck, n in NN_+, $ where $planck = h/(2pi)$, and $n$ is called _quantum number_.
]

Bohr has another ingenious idea #box(inset: (y: -.2em), image("image/Image_1766926670851_760.gif", height: 1.4em)) that the centripetal force is equal to the Coulomb force. Thus, solving the equation, we have $ r_n = n^2 (epsilon_0 h^2)/(pi m e^2),\ E_n = - 1/(8pi epsilon_0) e^2/r_n. $

(Around this chapter, we collected a new symbol $sigma$ #myDim("1/m") denoting _wavenumber_.)

The correct results of quantum mechanics:

$ myMathRect(
  &E_n = - 1/n^2 (m e^4)/(8 epsilon^2 h^2)\, &&n in NN_+\,\
  &L = sqrt(l (l+1)) planck\, &&l in NN inter [0,n)\,\
  &L_z = m_l planck\, &&m_l in ZZ inter [-l,l]\,\
  &#myEquationNote(buffer: none, wrap-f: x=>x)[Plus...#h(.5em)] S_z = m_s planck\, &&m_s in {plus.minus 1/2}.
) $

Of the equations above, $n, l, m_l, m_s$ are respectively 1) principal, 2) secondary, 3) magnetic, and 4) spin quantum number.

The $m_l$ has a close relationship with $l$. $l$ denotes the quantum number for the _magnitude_ of angular momentum and $m_l$ denotes the _$z$-component_ of it. (The direction of $z$ here is the direction of the external magnetic field $bold(B)$.) They form an $bold(L)$ together.

Plus, surrounding $L_z$ we can use some #strike[forgotten] knowledge in electromagnetism to get extras:

0. First, the conclusion is, for the electron "orbiting" around the nucleus, we have $ bold(mu) = (-e)/(2 m) bold(L), $ where $bold(mu)$ is the magnetic moment #emoji.magnet#emoji.axe, and $bold(L)$ is the angular momentum /*#emoji.tornado*/#emoji.boomerang.

1. The hypotheses are:
    - The orbit of the electron is a circle, with a radius $r$.
    - The electron orbits at a constant period $T$.

2. Then we can deduce: $
    bold(mu)
    &= I bold(S)
    = (Delta Q)_T/T dot pi bold(r)_x times bold(r)_y
    = (-e)/((2pi)/omega) dot pi r^2 bold(e_z)\
    &= (-e)/2 r^2 bold(omega)
    = (-e)/2 bold(r) times bold(v)\
    &= (-e)/(2 m) bold(L). #text(font: "Noto Emoji", h(.5em)+emoji.face.happy)
  $

3. Thus, $
    mu_z
    &= (-e)/(2 m) L_z\
    &= (-e)/(2 m) (m_l planck)
    &&= - m_l mu_B,\
    &&&m_l in ZZ inter [-l, l],
  $ where $mu_B$ #myDim("J/T") is called Bohr magneton.

4. Finally, _sturdy grass withstands high winds; true gold stands the test of fire._ #text(size: .6em)[(Not aimed at you. #emoji.cat.face.laugh)] In the external magnetic field $bold(B)$, each different orientation of the magnetic moment $bold(mu)$ can result in a different additional energy as this shows: $
    Delta E
    &= - bold(mu) dot bold(B)\
    &= - mu_z B
    &&= m_l mu_B B,\
    &&&m_l in ZZ inter [-l,l],
  $ where $z$ is the direction of $bold(B)$.

// 18446744000000000001. 1<<64

/ Zeeman effect: From the above, we can derive the Zeeman effect: The bigger $m_l$, the bigger $Delta E$. Therefore, an energy level originally determined by a pair of $n,l$ will split into $(2l+1)$ sub energy levels in a magnetic field.

We had to say a little more about the spin of electrons. It is quite similar to their orbiting: We can make a *bold* assumption that $ S = sqrt(s (s+1)) planck,\ S_z = m_s planck, $ where $S$ is the magnitude of spin angular momentum, $s$ is the spin quantum number, and $m_s$ is the spin magnetic quantum number. So, #text(fill: gray)[recalling that in orbital motion, when $l$ is determined, $m_l$ can have $(2l+1)$ different values,] for spin, since $m_s$ has only 2 different values (at any time), we can assert that $s=1/2$. (_Note:_ It is not that \#(possible values) of $s$ determines \#(possible values) of $m_s$, but the value of $s$ itself determines \#(possible values) of $m_s$.) So our conclusion is $ s = 1/2, #h(1em) m_s = plus.minus 1/2. $

Further, we have $ S &= sqrt(3)/2 planck, #myEquationNote[Unlike $L$, this is a constant!]\ S_z &= plus.minus 1/2 planck. #myEquationNote[Unlike $L_z$, \#(possible $S_z$) is an even number.] $

If we are looking for magnetic moment, we can still use $mu_z = (-e)/(2 m) L_z$.

#colbreak()

== Wave, start!

=== For a single particle...

$
  p = h/lambda,\
  E = h nu,
$ where $nu, lambda$ are determined by current $m, v$ (*Note:* instead of rest mass $m_0$).

$
  Delta p_x Delta x >= h/(4pi),\
  Delta E Delta t >= h/(4pi).
$

An application for the second:\ #myDef[In an atom, the average time for electrons to stay on an energy level #text(size: .8em)[(hereinafter abbreviated as $E_n$)] is referred to as the _average lifetime_ of that energy level.] So, _thread 1._ according to the energy-time uncertainty relation, the higher the average lifetime (that is, $Delta t$), the lower $Delta E$ (that is, the width of $E_n$); _thread 2._ according to common knowledge, the longer electrons stay on $E_n$, the more stable $E_n$ is. From the 2 threads we have a conclusion: The more stable $E_n$, the smaller $Delta E$, $<=>$ the more definite $E_n$.

As for specific applications, the ground state $E_1$ is the most definite, and due to $E_n arrow.b text(size: #.75em, => Delta t arrow.t) => Delta E arrow.b => Delta lambda_"Emitted"$, so it helps in selecting light emitting materials.

=== For mathematicians...

#show "Schrodinger": "Schrödinger"

$ mat(#[Wave-mechanics] ; #[Matrix-mechanics]) = mat(#[Schrödinger] ; #[Heisenberg, Bern, Pauli, etc.]) $

The book regards us as Schrodinger and we are going to build some #text(size: .75em, fill: gray)[basic] wave mechanics.

Actually, we only need one prerequisite knowledge point: for waves, a traditional _monochromatic plane wave_ can be expressed as $ y(x,t) = A "e"^(-"i"dot 2pi (nu t - x/lambda)). $

Similarly, for a free particle, its wave function is $ bold(Psi)(x,t) = bold(Psi)_0 "e"^(-"i"dot ...) = bold(Psi)_0 "e"^(-"i"/planck (E t - p x)), $ where #text(fill: gray)[(recap #emoji.face.happy)] $E = h nu$, $p = h/lambda$. *Note:* $bold(Psi)_0$ here is still to be determined (using the normalization method mentioned later).

The probability of a particle appearing in the volume element near $bold(r)$ at time $t$ is $ dd(W) = abs(bold(Psi)(bold(r),t))^2 dd(V), $ where we can substitute $abs(bold(Psi))^2$ as $bold(Psi) bold(Psi)^*$.

Wave functions must be 1) single-valued, 2) finite, 3) continuous, and 4) normalized ($integral.triple dd(W(bold(r),t)) = 1$).

#Showy.showybox(
  title: [Time-independent Schrodinger equation (Stationary Schrodinger equation)],
  frame: myShowyFrameBlue,
  ..myShowyDefaultParam
)[
  $ (partial^2/(partial x^2)+partial^2/(partial y^2)+partial^2/(partial z^2)) bold(Psi)(bold(r)) +\ (2 m (E - V))/(planck^2) bold(Psi)(bold(r)) = 0, $ where $bold(Psi)(bold(r))$ is simplified from $bold(Psi)(bold(r),t)$; $E$ is the energy of the particle, and $V(bold(r))$ is the potential energy #text(size: .75em)[(for example, in hydrogen atom, $V(bold(r)) = - (e^2)/(4pi epsilon_0 r)$; in one-dimensional harmonic oscillator, $V(bold(r)) = 1/2 m omega^2 x^2$)].
][
  // ☞ 
  Example -- One-dimensional infinite well:

  $bold(Psi)(x)$ satisfies $ cases(
    Ph.derivative(bold(Psi)(x), x, 2) + k^2 bold(Psi)(x) = 0\, &(#box(width: 44%, baseline: 40%-5%)[Within the potential well's range $(0,a)$,]),
    bold(Psi)(x) = 0\, &x in (-infinity, 0] union [a, +infinity)\,
  ) $ where $k=sqrt(2 m E)/planck$.

  The first equation has the general solution $bold(Psi)(x) = A sin k x + B cos k x$. Using the 2 boundary conditions, we get $ bold(Psi)(x) = A sin k x, $ and $k = (n pi)/a = sqrt(2 m E)/planck$. Further substitution yields $ bold(Psi)_n (x) = plus.minus sqrt(2/a) sin(n pi x/a), n in NN_+. $

  So, we can also know the wave length and energy of the particle at some energy level $n in NN_+$: $
    lambda_n = (2 a)/n,\
    E_n = n^2 (h^2)/(8 m a^2). 
  $
  
  There are several types of $V(x)$, including $C$, infinite/finite well, step potential, potential barrier, and (harmonic) oscillator. It is surprising that particles have a certain probability to enter the regions where $E<V$; for harmonic oscillators, the zero-point energy is $1/2 h nu$ ($E_n = (n+1/2) h nu$), instead of $0$ (cases are free particle, step potential, and potential barrier, but their energies are all continuous #emoji.face.joy, i.e. they are in an unbound state).
]

== Let's build an atom!

=== First, deal with the electrons in the shell structure

In an atom, $(n,k,m_l,m_s)$ determines a unique electron. $n$-th shell layer contains at most $Z_n = sum_(l=0)^(n-1) (2l+1) times 2 = 2n^2$ electrons; a sub-layer with some $l$ can contain at most $(2l+1) times 2$ electrons.

(The end, because we've already learned how to build an atom's shell in high school chemistry, e.g. Pauli exclusion principle and the principle of minimum energy. ...)

// == Materials are said to be one of the four major pitfalls...
= Solid State Physics

== Bands

/ Energy band: A group of an split energy level.
/ Full band: All energy levels within it are filled with $e$'s.
/ Valence band: Formed by split valence levels. (Usually has the highest energy.)
/ Empty band: Levels corresponding to excited states of an atom. Is empty if the atom is at the unexcited state.
/ Conduction band: Not filled with $e$'s.
/ Band gap (Forbidden band): Is *NOT* an energy band! As its name reveals, it is the gap between two adjacent band.

== Three-body (?)

(Here we focus on crystals only.)

/ Insulators:
  
  Their highest valence band is filled with valence $e$'s, and thus becomes a _full band_. And it is far from the nearest empty band above it, with a distance of $Delta E_g = 3~6 #unit("eV")$.

/ Conductors:

  There are 3 types of band structures:
  #enum(numbering: "a)")[
    Valence band is not fully filled.\ Example: Li.
  ][
    Valence band is connected, or partially overlaps with an empty band.\ Example: Mg, Be, Zn. Taking Mg, its $e$'s fill up to 3s, and its 3s overlaps with 3p. Hence, $e$'s on 3s can jump to 3p's energy levels. So the 3s+3p are filled 2/8 (sounds like hybrid orbitals?), and thus Mg is a good conductor.
  ][
    Adding #numbering("a)", 1) and #numbering("a)", 2) together. Some metals' valence bands are not fully filled and overlap with empty bands.\ Example: Na, K, Al, Cu, Ag.
  ]

/ Semiconductors:

  Their band structures are similar to insulators', but the $Delta E_g$-s are much smaller, at around $0.1~1.5 #unit("eV")$. The "hole"s left by the excited $e$'s (which are usually from the top of the full band and to the empty band) are familiar to us. #emoji.face.friendly

// ]