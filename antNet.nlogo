breed [nodes node]
breed [ants ant]
breed [packets packet]

nodes-own [
  pheromone-table ;; list where each entry is a list in form [t n v], where t is the target node, n is a linked node and v is a value about the goodness of going to t through n.
  generation-table ;;list where each entry is a list in form [g h tt fh ts], where g is the ant generation, h is the number of hops of the best ant, tt is its travel time, fh its first hop and ts is the timestamp about when the entry eas accessed last time

  ;; the following are just for sake of simulation
  target-dest
  next-send-time
  messages-in-a-row
  buffered-packets

  next-hello-mess ; how many ticks to wait before sending it?
]

packets-own[
  target-node
  owner-node
  next
  previous
  message
  waiting?

  hello-message?
]

ants-own[
  target-node
  owner-node
  next
  previous
  visited-nodes
  proactive-ant?
  forward-ant?
  n-hops
  travel-time
  generation
  time-to-target
  n_broadcasts ; useful just to proactive ants
]

globals[
  alpha_1_factor ;;the tollerance for number of hops and best travel time, for ants which first hop was the same of the best ant's one
  alpha_2_factor ;;the tollerance for number of hops and best travel time, for ants which first hop was not the same of the best ant's one
  ant-generation-memory
  t_hop ; param which represent the time to take one hop in unloaded conditions.
  gamma_factor ; param in [0,1] which represent how much the memory of pheromone table is static respect new data. 1 means that pheremone table doesnt update.
  ;ant-every-n-packets ; after many data packets should we send a proactive ant?
  ;proactive-broadcast-prob ; probability of a proactive ant to be broadcasted by a node
  max-proactive-ant-broadcasts ; the max number a proactive ant can be broadcasted
  ;interval-hello-messages ; how often to send an hello message to the neighbors?
]


to setup
  clear-all

  ;;;;;; setup of global variables
  set alpha_1_factor 0.9
  set alpha_2_factor 2.0
  set ant-generation-memory 1000
  set t_hop 3e-3
  set ant-every-n-packets 5
  set max-proactive-ant-broadcasts 2

  setup-nodes
  reset-ticks
end

to setup-nodes
  create-nodes 20 [
    setxy random-xcor random-ycor
    set color blue
    set size 5
    set pheromone-table []
    set generation-table []
    set target-dest nobody
    set next-send-time 0
    set messages-in-a-row 0
    set buffered-packets []
    set next-hello-mess random hello-mess-interval
  ]
  ask node 0 [
  set color red
  ]

  ask node 5 [
  set color cyan
  ]

  ask node 10 [
  set color yellow
  ]
end


to go
  update-nodes
  update-links
  update-ants
  update-packets
  tick
end



to update-nodes

  ;;eventually, update the network
  if (net-evolution)
  [
    ask nodes [
      rt (random 10 - 5) * movement-speed ; random turn
      fd movement-speed ; move forward
    ]
  ]

  ask nodes[
    foreach generation-table [ ? ->
      if item 4 ? < ticks - ant-generation-memory
      [
        let index position ? generation-table
        set generation-table remove-item index generation-table
      ]
    ]

    ifelse next-hello-mess = 0
    [
      set next-hello-mess hello-mess-interval
      send-hello-message
    ]
    [set next-hello-mess next-hello-mess - 1]

    ;;check if user chose to use this node as sender
    ifelse (connection_1 and node source_node_1 = self)
    [
      if target-dest != node dest_node_1
      [
        set target-dest node dest_node_1
        set next-send-time ticks
        set messages-in-a-row 0
      ]
    ]
    [
      ifelse (connection_2 and node source_node_2 = self)
      [
        if target-dest != node dest_node_2
        [
          set target-dest node dest_node_2
          set next-send-time ticks
          set messages-in-a-row 0
        ]
      ]
      [set target-dest nobody]
    ]


    ;; if you have a target and enough time passed, send him something
    if (not (target-dest = nobody)) and next-send-time = ticks
    [
      let way get-way-for-target-node target-dest
      ifelse way = nobody
      [
        send-forward-ant false
        set next-send-time next-send-time + 5 * packet-interval
      ]
      [
        ifelse messages-in-a-row < ant-every-n-packets
        [
          send-data-packet
          set messages-in-a-row messages-in-a-row + 1
        ]
        [
          send-forward-ant true
          set messages-in-a-row 0
        ]

        set next-send-time next-send-time + packet-interval

      ]


    ]
  ]
end

to send-hello-message
  ;; to be executed by a node
  let source self
  let next-nodes [self] of link-neighbors

  foreach next-nodes [ id ->
    hatch-packets 1[
      set color [color] of source
      set size 2
      set target-node id
      set owner-node source
      set next id
      set previous source
      set waiting? false
      set hello-message? true
      set shape "square 2"
    ]
  ]
end

to send-data-packet
  ;; to be executed by a node
  let source self
  let target target-dest
  let nextnode get-way-for-target-node target

  hatch-packets 1[
    set color [color] of source

    set size 2
    set target-node target
    set owner-node source
    set next nextnode
    set previous source
    set waiting? false
    set hello-message? false
    set shape "circle 2"
  ]

end

to send-forward-ant [is_proactive?]
  ;; to be executed by a node

  ;;check if we already have data for this target
  let source self
  let target target-dest

  let nextnode get-way-for-target-node target
  let g random 1000000

  let next-nodes []

  let will_be_broadcasted? false
  if is_proactive?
  [
    let random-value random-float 1.0
    if random-value <= proactive-broadcast-prob
    [set will_be_broadcasted? true]
  ]
  ifelse (nextnode = nobody) or will_be_broadcasted?
  [
    set will_be_broadcasted? true
    ;let neighbors-excluding-node filter [neighbor -> neighbor != sender and neighbor != ant-owner] link-neighbors
    set next-nodes [self] of link-neighbors
  ]
  [set next-nodes lput nextnode next-nodes]


  foreach next-nodes [ id ->
    ; Create a new ant for each link
    hatch-ants 1[
      set color pink
      set shape "bug"
      set size 3
      set forward-ant? true
      set proactive-ant? is_proactive?
      setxy [xcor] of source [ycor] of source
      face id
      set owner-node source
      set previous source
      set next id
      set target-node target
      set n-hops 0
      set travel-time 0
      set generation g
      set time-to-target 0
      set visited-nodes (list source)

      ifelse will_be_broadcasted?
      [set n_broadcasts 1]
      [set n_broadcasts 0]
  ]
  ]

end

to-report get-way-for-target-node [target]
  ;; To be executed by a node when it asks itslef which way to take in order to reach the node "target".
  ;; if the return value is nobody, then the "query-node" has no clue. Otherwise, the returned value is the next node to ask for, closer to target.


  let nodes-list []
  let goodness-list []
  foreach pheromone-table[ ? ->
    if item 0 ? = target[
      let n item 1 ?
      let v item 2 ?

      set nodes-list lput n nodes-list
      set goodness-list lput v goodness-list
    ]
  ]

  if length nodes-list = 0 [report nobody] ;if we are here, we have no clue about how to reach the target node

  report sample-element nodes-list goodness-list
end

to-report sample-element [n p] ;it returns one of the possibility stochastichally, based on input weights for each possibility
  let total-probability sum p
  let random-value random-float total-probability

  let cumulative-probability 0
  let selected-element nobody

  foreach p [ ? ->
    if random-value >= cumulative-probability and random-value < cumulative-probability + ? [
      let index position ? p
      set selected-element item index n
      report selected-element
    ]
    set cumulative-probability cumulative-probability + ?
  ]
end


to update-ants
  ask ants[
    let nxt-node next
    let a self
    ask previous
    [
      if not link-neighbor? nxt-node
      [ask a[die]]
    ]
    set travel-time travel-time + 1
    if not forward-ant?
    [set time-to-target time-to-target + 1]

    face next
    let delta_s 50 * movement-speed
    if distance next <= delta_s
    [
      set delta_s distance next
    ]
    forward delta_s

    if (pxcor = [pxcor] of next) and (pycor = [pycor] of next)
    [
      ask next[new-ant-arrived-to-node a]
    ]
  ]
end


to update-packets
  ask packets[
    let this_packet self
    if not waiting?
    [
      let nxt-node next
      let a self
      ask previous
      [
        if not link-neighbor? nxt-node
        [ask a[die]]
      ]

      face next
      let delta_s 50 * movement-speed
      if distance next <= delta_s
      [
        set delta_s distance next
      ]
      forward delta_s

      if (pxcor = [pxcor] of next) and (pycor = [pycor] of next)
      [
        ifelse next = target-node
        [die]
        [
          ask next [new-packet-arrived-to-node this_packet]
        ]
      ]
    ]
  ]
end

to new-packet-arrived-to-node [p]
  ;;to be executed by a node, when the packet p arrives at it.
  let this_node self
  let nextnode get-way-for-target-node [target-node] of p
  ifelse nextnode = nobody
  [
    ask p [set waiting? true]
    set buffered-packets lput p buffered-packets
  ]
  [
    ask p
    [
      set next nextnode
      set previous this_node
    ]
  ]

end

to new-ant-arrived-to-node [a]
  ;;to be executed by a node, when the ant a arrives at it.
  let this_node self

  if [forward-ant?] of a
  [
    ask a[set visited-nodes lput [next] of a visited-nodes]

    if [target-node] of a = this_node
    [
      ;print "got it"
      ask a
      [
        set forward-ant? false
        set color lime

        set previous this_node
        let i length visited-nodes - 2
        set next item i visited-nodes
        ;stop
      ]
      stop
    ]

    let g [generation] of a
    let h [n-hops] of a
    let tt [travel-time] of a
    let fh item 1 [visited-nodes] of a

    let found 0
    foreach generation-table[ ? ->
      if item 0 ? = g and found = 0
      [
        let best-h item 1 ?
        let best-tt item 2 ?
        let first-hop-best item 3 ?

        if (h < best-h and tt < best-tt)
        [
          set best-h h
          set best-tt tt

          set first-hop-best fh
        ]
        let index position ? generation-table
        let new-entry (list g best-h best-tt first-hop-best ticks)

        ;; let's update the entry with the updated data.
        set generation-table replace-item index generation-table new-entry

        let alpha alpha_1_factor
        if fh != first-hop-best [set alpha alpha_2_factor]

        if (h > best-h * alpha or tt > best-tt * alpha)
        [
          ask a[die]
          stop
        ]
        set found 1
    ]]
    if found = 0
    [
      let new-entry (list g h tt fh ticks)
      set generation-table lput new-entry generation-table
    ]

    ;now, let's make the ant forward. We do it by cloning the previous ant as many times as needed, and killing the previous one.


    let nextnode get-way-for-target-node [target-node] of a
    let ant-owner [owner-node] of a
    let sender [previous] of a
    let next-nodes []

    let will_be_broadcasted? false
    if [proactive-ant?] of a
    [
      let random-value random-float 1.0
      if random-value <= proactive-broadcast-prob
      [set will_be_broadcasted? true]
    ]
    ifelse (nextnode = nobody) or will_be_broadcasted?
    [
      set will_be_broadcasted? true
      ;let neighbors-excluding-node filter [neighbor -> neighbor != sender and neighbor != ant-owner] link-neighbors
      set next-nodes [self] of link-neighbors with [self != sender and self != ant-owner]
    ]
    [set next-nodes lput nextnode next-nodes]

    if (not will_be_broadcasted?) or ([n_broadcasts] of a <= max-proactive-ant-broadcasts)
    [
      foreach next-nodes[ id ->
        hatch-ants 1[
          set color [color] of a
          set shape [shape] of a
          set size [size] of a
          set forward-ant? [forward-ant?] of a
          setxy [xcor] of a [ycor] of a
          set owner-node [owner-node] of a
          set target-node [target-node] of a
          set travel-time [travel-time] of a
          set generation [generation] of a

          set n-hops [n-hops] of a + 1
          set previous [next] of a
          set visited-nodes [visited-nodes] of a
          ;;set visited-nodes lput [next] of a visited-nodes  THIS property was set earlier
          set next id
          face next

          set proactive-ant? [proactive-ant?] of a

          let old_val [n_broadcasts] of a
          ifelse will_be_broadcasted?
          [set n_broadcasts old_val + 1]
          [set n_broadcasts old_val]
        ]

      ]
    ]
    ask a[die] ;; the old ant is killed, after being cloned.
    stop
  ];;if it was a forward ant


  if not [forward-ant?] of a
  [
    ;;in this case, we are backward ants

    update-pheromone-table a

    ifelse [owner-node] of a = this_node
    [
      ;print "arrived home"
      ask a [die]
    ]
    [
      ask a
      [
        let index position this_node visited-nodes
        set index index - 1
        set previous this_node
        set next item index visited-nodes
      ]
    ]
  ]


end

to update-pheromone-table [a]
  ;;to be invoked by a node, when it receives a backward ant
  let this_node self
  let target [target-node] of a
  let nxt [previous] of a ;NOTE: it is previous 'cause we still didn't update the reference with the new one.
  let t-time [time-to-target] of a
  let hops length [visited-nodes] of a - position this_node [visited-nodes] of a - 1

  ;; this value represents a measure about how good it is taking this route (so far) to go to target-node
  let route-value 2 / (t-time + (hops * t_hop))

  foreach pheromone-table [ ? ->
    if item 0 ? = target and item 1 ? = nxt
    [
      ;let's update this value
      let old_value item 2 ?
      let new-value gamma_factor * old_value + ((1 - gamma_factor) * route-value)

      let index position ? pheromone-table
      let new-entry (list target nxt new-value)

      ;; let's update the entry with the updated data.
      set pheromone-table replace-item index pheromone-table new-entry
      stop
    ]
  ]

  ; if we are here, then this entry was not present in the table. Let's add it.
  let new-entry (list target nxt route-value)
  set pheromone-table lput new-entry pheromone-table

  ;; finally, if we were buffering packtes for that destination, let's free them
  foreach buffered-packets[ ? ->
    if [target] of ? = target
    [
      ask ? [set waiting? false]
      let index position ? buffered-packets
      set buffered-packets remove-item index buffered-packets
    ]
  ]

end


to update-links
  ask nodes [
    ; Check other turtles and create a link if it doesn't already exist and they are within the distance threshold
    ask other nodes [
      if not link-neighbor? myself and distance myself <= distance-threshold
      [
        create-link-with myself
      ]
    ]

    ask links[
      if link-length > distance-threshold [die]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
702
10
1316
625
-1
-1
6.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
0
0
1
ticks
30.0

BUTTON
56
47
129
80
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
257
47
320
80
NIL
go\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
54
149
231
182
distance-threshold
distance-threshold
1
75
27.0
1
1
NIL
HORIZONTAL

BUTTON
191
47
254
80
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
237
149
415
182
packet-interval
packet-interval
1
60
30.0
1
1
NIL
HORIZONTAL

SLIDER
55
188
231
221
movement-speed
movement-speed
0.01
0.1
0.02
0.01
1
NIL
HORIZONTAL

SWITCH
54
226
231
259
net-evolution
net-evolution
1
1
-1000

CHOOSER
211
449
349
494
source_node_1
source_node_1
0 5 10
2

CHOOSER
353
449
491
494
dest_node_1
dest_node_1
0 5 10
0

TEXTBOX
55
351
193
399
Node 0: Red 
20
15.0
1

SWITCH
51
454
205
487
connection_1
connection_1
0
1
-1000

SWITCH
51
515
205
548
connection_2
connection_2
1
1
-1000

CHOOSER
211
510
349
555
source_node_2
source_node_2
0 5 10
0

CHOOSER
353
510
491
555
dest_node_2
dest_node_2
0 5 10
1

TEXTBOX
54
376
204
400
Node 5: Cyan
20
85.0
1

TEXTBOX
53
400
234
433
Node 10: Yellow
20
44.0
1

SLIDER
236
188
416
221
hello-mess-interval
hello-mess-interval
50
150
50.0
10
1
NIL
HORIZONTAL

SLIDER
422
149
646
182
proactive-broadcast-prob
proactive-broadcast-prob
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
235
226
416
259
ant-every-n-packets
ant-every-n-packets
1
10
5.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
