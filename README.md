<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline6">1. Brahmin Runner</a>
<ul>
<li><a href="#orgheadline1">1.1. Vorraussetzungen</a></li>
<li><a href="#orgheadline2">1.2. Kompilieren</a></li>
<li><a href="#orgheadline5">1.3. Benutzen</a>
<ul>
<li><a href="#orgheadline3">1.3.1. Wettbewerbsmodus</a></li>
<li><a href="#orgheadline4">1.3.2. Validitätsprüfung</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#orgheadline7">2. Zusatzbemerkungen</a></li>
</ul>
</div>
</div>


# Brahmin Runner<a id="orgheadline6"></a>

Ein kleines Tool welches ermöglicht die eigenen Teilnehmerprogramme für den [PWB
im Sommersemester 2016](https://pwb.asta-wedel.de/SS16-BrahminPacker/index.html) unter Wettbewerbsnahen Bedingungen zu testen.

## Vorraussetzungen<a id="orgheadline1"></a>

-   Es wird eine [Erlang Umgebung](http://www.erlang.org/) benötigt, mindestens in Version `16B3` benötigt.
-   Für das kompilieren wird ausserdem [`rebar3`](http://www.rebar3.org/) benötigt.<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>

## Kompilieren<a id="orgheadline2"></a>

    rebar3 escriptize # Es werden Abhängigkeiten heruntergeladen und kompiliert
    cp _build/default/bin/brahmin_runner $euer_projekt_pfad

## Benutzen<a id="orgheadline5"></a>

Im Moment gibt es zwei Modi der Nutzung.

### Wettbewerbsmodus<a id="orgheadline3"></a>

    brahmin_runner <zeit> <problem>

Führt euer Programm unter Wettbewerbsähnlichen Bedingungen aus.

Dazu wird die Datei `problems/<problem>.prb` eingelesen und euer Programm durch
einen Aufruf von `make run` gestartet. Nach einem kurzen Countdown (5 Sekunden)
erhält euer Programm seine Daten über `stdin`.

Wärend der nun folgenden <zeit> Sekunden wird euch angezeigt wenn euer Programm
eine Lösung einreicht und eine Bewertung dieser Lösung im Vergleich zum
bisherigen Bestwert.

### Validitätsprüfung<a id="orgheadline4"></a>

    brahmin_runner validate <problem>

Überprüft ob ein gegebenes Problem (ebenfalls `problems/<problem>.prb`) nach den
Regeln in einem gültigen Format vorliegt.

# Zusatzbemerkungen<a id="orgheadline7"></a>

Überprüfung eurer Lösung auf Gültigkeit und Berechnung der Punkte erfolgt mit
dem gleichen Code wie im Wettbewerb auch, ebenfalls der Parser wird der gleiche
sein. Sollte also eine Lösung von euch nicht angenommen werden, die eurer
Meinung nach korrekt wäre, dann bitte eine Meldung an mich. Allerdings werde ich
aus Fairness ab dem Monatswechsel am Parsing nur noch in Notfällen etwas ändern.

<div id="footnotes">
<h2 class="footnotes">Footnotes: </h2>
<div id="text-footnotes">

<div class="footdef"><sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> <div class="footpara">Unter Umständen werdet ihr `rebar3` selbst kompilieren müssen, damit es
zu eurer Erlang Version passt.</div></div>


</div>
</div>
