# ScalaBigraph #

Scala Bigraph is a Scala implementation of Robin Milner's Bigraph model.
A Bigraph is a data structure that entails two graphical structures: 
a forest and a hypergraph.
The forest is used to model nested location of nodes while the 
hypergraph is used to model connectivity between nodes.

The library follows as faithfully as possible the syntax and semantics 
defined in [1] with some modifications to make the mathematical model
easy to use as a scala library. We pinpoint these modifications in the 
Details section below. 

## Example: ##

## Details: ##

 - Place Graphs and Link Graphs independently in two different traits, 
 `PlaceGraph` and `LinkGraph`, and combined in a third trait called `Bigraph`. 
 This gives the flexibility for a user to model only placing structures, 
 linking structures, or both.

 - The library currently supports concrete bigraphs, i.e., nodes and edges 
   are named. In bigraphical terminology this is known as *support*. Node
   support is modelled generically, i.e., `Bigraph[A]` implements a bigraph
   with nodes of type `A`. Edges do not currently have a generic type. 

## TODO List: ##

 - More than a data structure, the bigraphical framework is a 
   model of computation named Bigraph Reactive Systems (BRS). 
   The dynamics is modelled using reaction rules that define how a 
   given match in a bigraph can change. 
   The next step for ScalaBigraph is to provide an implementation of BRS.
 
 - Robin Milner provides in [1] a semantics for bigraphs without support, 
   i.e., nodes and edges are not named. These are called Abstract Bigraphs.
   An implementation of Abstract Bigraphs is also an objective for 
   ScalaBigraph.
 

This library is a personal project aimed primarily, but not exclusively, 
at the Bigraph community. This is still a work in progress and feedback 
is more than welcome! 

[1] R. Milner, The Space and Motion of Communicating Agents. 
Cambridge University Press, 2009.

Developer: Eloi Pereira, eloi@berkeley.edu, http://eloipereira.com
