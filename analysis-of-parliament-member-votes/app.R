if (!require(shiny)) install.packages("shiny", dependencies = TRUE);

source("tools.R")

ustawy <-c("administracji podatkowej", "bateriach i akumulatorach oraz niektórych innych ustaw",
"bezpieczeństwie imprez masowych oraz niektórych innych ustaw",
"bezpieczeństwie morskim oraz niektórych innych ustaw", "biokomponentach i biopaliwach ciekłych oraz niektórych innych ustaw",
"charakterystyce energetycznej budynków", "cudzoziemcach", "cudzoziemcach oraz ustawy o promocji zatrudnienia i instytucjach rynku pracy",
"dochodach jednostek samorządu terytorialnego", "dokumentach paszportowych",
"dotacji przeznaczonej dla niektórych podmiotów", "drogach publicznych",
"drogach publicznych oraz niektórych innych ustaw", "drogach publicznych oraz ustawy o autostradach płatnych oraz o Krajowym Funduszu Drogowym",
"działach administracji rządowej oraz niektórych innych ustaw",
"działaczach opozycji antykomunistycznej oraz osobach represjonowanych z powodów politycznych",
"działalności leczniczej oraz zmianie niektórych innych ustaw",
"działalności pożytku publicznego i o wolontariacie oraz niektórych innych ustaw",
"działalności pożytku publicznego i o wolontariacie oraz ustawy o fundacjach",
"emeryturach i rentach z Funduszu Ubezpieczeń Społecznych",
"emeryturach i rentach z Funduszu Ubezpieczeń Społecznych oraz niektórych innych ustaw",
"ewidencji ludności i dowodach osobistych oraz niektórych innych ustaw",
"finansach publicznych", "finansach publicznych oraz niektórych innych ustaw",
"funkcjonowaniu banków spółdzielczych", "funkcjonowaniu górnictwa węgla kamiennego w latach 2008",
"gospodarce opakowaniami i odpadami opakowaniowymi", "grach hazardowych",
"informatyzacji działalności podmiotów realizujących zadania publiczne oraz niektórych innych ustaw",
"inwestycjach w zakresie terminalu regazyfikacyjnego skroplonego gazu ziemnego w Świnoujściu oraz ustawy o gospodarce nieruchomościami",
"izbach rolniczych oraz niektórych innych ustaw", "jednostkach doradztwa rolniczego",
"kierujących pojazdami", "Kodeks cywilny", "Kodeks cywilny oraz niektórych innych ustaw",
"Kodeks karny", "Kodeks karny oraz niektórych innych ustaw",
"Kodeks postępowania cywilnego", "Kodeks postępowania cywilnego oraz niektórych innych ustaw w związku ze wspieraniem polubownych metod rozwiązywania sporów",
"Kodeks postępowania cywilnego oraz ustawy o kosztach sądowych w sprawach cywilnych",
"Kodeks postępowania karnego", "Kodeks postępowania karnego oraz niektórych innych ustaw",
"Kodeks postępowania w sprawach o wykroczenia", "Kodeks pracy",
"Kodeks pracy oraz niektórych innych ustaw", "Kodeks pracy oraz ustawy o związkach zawodowych",
"Kodeks spółek handlowych oraz niektórych innych ustaw", "Kodeks wyborczy",
"Kodeks wyborczy oraz niektórych innych ustaw", "komisjach lekarskich podległych ministrowi właściwemu do spraw wewnętrznych",
"komornikach sądowych i egzekucji", "kontroli niektórych inwestycji",
"Krajowej Szkole Sądownictwa i Prokuratury oraz niektórych innych ustaw",
"Krajowej Szkole Sądownictwa i Prokuratury oraz ustawy", "Krajowym Rejestrze Sądowym oraz o niektórych innych ustaw",
"kształtowaniu ustroju rolnego", "kształtowaniu ustroju rolnego oraz o zmianie niektórych innych ustaw",
"lasach", "leczeniu niepłodności", "listach zastawnych i bankach hipotecznych oraz niektórych innych ustaw",
"mniejszościach narodowych i etnicznych oraz o języku regionalnym oraz ustawy o działach administracji rządowej",
"nadzorze nad rynkiem finansowym", "nadzorze nad rynkiem finansowym oraz niektórych innych ustaw",
"nadzorze uzupełniającym nad instytucjami kredytowymi", "narodowym zasobie archiwalnym i archiwach",
"nasiennictwie", "niektórych formach popierania budownictwa mieszkaniowego oraz niektórych innych ustaw",
"niektórych umowach zawieranych w związku z realizacją zamówień o podstawowym znaczeniu dla bezpieczeństwa państwa",
"nieodpłatnej pomocy prawnej oraz edukacji prawnej", "obligacjach",
"obrocie instrumentami finansowymi oraz innych ustaw", "obrocie instrumentami finansowymi oraz niektórych innych ustaw",
"obrocie z zagranicą towarami", "ochronie gruntów rolnych i leśnych",
"ochronie i pomocy dla pokrzywdzonego i świadka", "ochronie przyrody oraz ustawy",
"ochronie zabytków i opiece nad zabytkami oraz ustawy o muzeach",
"ochronie zwierząt wykorzystywanych do celów naukowych lub edukacyjnych",
"odnawialnych źródłach energii", "odpadach", "odpadach oraz niektórych innych ustaw",
"odpadach wydobywczych oraz niektórych innych ustaw", "odwróconym kredycie hipotecznym",
"ofercie publicznej i warunkach wprowadzania instrumentów finansowych do zorganizowanego systemu obrotu oraz o spółkach publicznych oraz niektórych innych ustaw",
"organizacji rynku rybnego", "organizmach genetycznie zmodyfikowanych oraz niektórych innych ustaw",
"płatnościach w ramach systemów wsparcia bezpośredniego",
"podatku akcyzowym", "podatku dochodowym od osób fizycznych",
"podatku dochodowym od osób fizycznych oraz ustawy o swobodzie działalności gospodarczej",
"podatku dochodowym od osób prawnych", "podatku od towarów i usług oraz niektórych innych ustaw",
"podatku od towarów i usług oraz ustawy", "podatku od towarów i usług oraz ustawy o zwrocie osobom fizycznym niektórych wydatków związanych z budownictwem mieszkaniowym",
"podatku od wydobycia niektórych kopalin", "Policji", "Policji oraz niektórych innych ustaw",
"pomocy państwa w nabyciu pierwszego mieszkania przez ludzi młodych",
"pomocy społecznej", "postępowaniu wobec osób z zaburzeniami psychicznymi stwarzających zagrożenie życia",
"powszechnym obowiązku obrony Rzeczypospolitej Polskiej oraz niektórych innych ustaw",
"prawach konsumenta", "prawie autorskim i prawach pokrewnych oraz ustawy o grach hazardowych",
"Prawo budowlane oraz niektórych innych ustaw", "Prawo energetyczne",
"Prawo energetyczne oraz niektórych innych ustaw", "Prawo farmaceutyczne oraz niektórych innych ustaw",
"Prawo geodezyjne i kartograficzne", "Prawo geologiczne i górnicze oraz niektórych innych ustaw",
"Prawo lotnicze", "Prawo o adwokaturze oraz niektórych innych ustaw",
"Prawo o postępowaniu przed sądami administracyjnymi", "Prawo o ruchu drogowym",
"Prawo o ruchu drogowym oraz niektórych innych ustaw", "Prawo o ruchu drogowym oraz ustawy",
"Prawo o szkolnictwie wyższym oraz niektórych innych ustaw",
"Prawo o ustroju sądów powszechnych", "Prawo o ustroju sądów powszechnych oraz niektórych innych ustaw",
"Prawo o ustroju sądów wojskowych", "Prawo o zgromadzeniach",
"Prawo ochrony środowiska oraz niektórych innych ustaw", "Prawo prasowe",
"Prawo telekomunikacyjne oraz niektórych innych ustaw", "Prawo upadłościowe i naprawcze",
"Prawo własności przemysłowej", "Prawo własności przemysłowej oraz niektórych innych ustaw",
"Prawo wodne oraz niektórych innych ustaw", "Prawo zamówień publicznych",
"Prawo zamówień publicznych oraz niektórych innych ustaw",
"Prawo zamówień publicznych oraz ustawy o koncesji na roboty budowlane lub usługi",
"prokuraturze", "promocji zatrudnienia i instytucjach rynku pracy oraz niektórych innych ustaw",
"przygotowaniu finałowego turnieju Mistrzostw Europy w Piłce Nożnej UEFA EURO 2012",
"rachunkowości", "rachunkowości oraz niektórych innych ustaw",
"radiofonii i telewizji", "recyklingu pojazdów wycofanych z eksploatacji oraz niektórych innych ustaw",
"redukcji niektórych obciążeń administracyjnych w gospodarce",
"refundacji leków", "rodzinnych ogrodach działkowych", "rozpatrywaniu reklamacji przez podmioty rynku finansowego i o Rzeczniku Finansowym",
"rybołówstwie morskim", "rzeczach znalezionych", "Sądzie Najwyższym",
"samorządzie gminnym", "samorządzie gminnym oraz niektórych innych ustaw",
"samorządzie gminnym oraz o zmianie niektórych innych ustaw",
"składkach na ubezpieczenie zdrowotne rolników za 2012 r.",
"skutkach powierzania wykonywania pracy cudzoziemcom przebywającym wbrew przepisom na terytorium Rzeczypospolitej Polskiej",
"Służbie Celnej", "spółdzielczych kasach oszczędnościowo",
"sporcie", "środkach ochrony roślin", "środkach przymusu bezpośredniego i broni palnej",
"substancjach zubożających warstwę ozonową oraz o niektórych fluorowanych gazach cieplarnianych",
"świadczeniach opieki zdrowotnej finansowanych ze środków publicznych oraz niektórych innych ustaw",
"świadczeniach rodzinnych", "świadczeniach rodzinnych oraz niektórych innych ustaw",
"systemie monitorowania i kontrolowania jakości paliw", "systemie monitorowania i kontrolowania jakości paliw oraz niektórych innych ustaw",
"systemie oświaty", "systemie oświaty oraz niektórych innych ustaw",
"systemie oświaty oraz ustawy o zmianie ustawy o systemie oświaty oraz o zmianie niektórych innych ustaw",
"systemie ubezpieczeń społecznych oraz niektórych innych ustaw",
"szczególnych rozwiązaniach dla pracowników i przedsiębiorców na rzecz ochrony miejsc pracy",
"szczególnych zasadach przygotowania i realizacji inwestycji w zakresie dróg publicznych",
"szczególnych zasadach przygotowania i realizacji strategicznych inwestycji w zakresie sieci przesyłowych",
"terminach zapłaty w transakcjach handlowych", "transporcie drogowym",
"transporcie drogowym oraz niektórych innych ustaw", "transporcie drogowym oraz ustawy o czasie pracy kierowców",
"transporcie kolejowym", "transporcie kolejowym oraz niektórych innych ustaw",
"transporcie kolejowym oraz ustawy o zmianie ustawy o transporcie kolejowym",
"Trybunale Konstytucyjnym", "uchyleniu ustawy o ustanowieniu programu wieloletniego \"Program dla Odry",
"udostępnianiu informacji o środowisku i jego ochronie", "udzielaniu cudzoziemcom ochrony na terytorium Rzeczypospolitej Polskiej oraz niektórych innych ustaw",
"ułatwieniu dostępu do wykonywania niektórych zawodów regulowanych",
"ułatwieniu wykonywania działalności gospodarczej", "urzędzie Ministra Obrony Narodowej oraz niektórych innych ustaw",
"usługach płatniczych oraz niektórych innych ustaw", "ustaleniu i wypłacie zasiłków dla opiekunów oraz o poselskich projektach ustaw o zmianie ustawy o świadczeniach rodzinnych",
"utrzymaniu czystości i porządku w gminach oraz niektórych innych ustaw",
"utrzymaniu czystości i porządku w gminach oraz o zmianie ustawy o własności lokali",
"utworzeniu Polskiej Agencji Kosmicznej", "wspieraniu rodziny i systemie pieczy zastępczej",
"wspieraniu rodziny i systemie pieczy zastępczej oraz niektórych innych ustaw",
"wykonywaniu kary pozbawienia wolności poza zakładem karnym w systemie dozoru elektronicznego",
"wyrobach budowlanych", "wyrobach budowlanych oraz ustawy", "wzajemnej pomocy przy dochodzeniu podatków",
"zagospodarowaniu wspólnot gruntowych", "zakwaterowaniu Sił Zbrojnych Rzeczypospolitej Polskiej",
"zaopatrzeniu emerytalnym żołnierzy zawodowych oraz ich rodzin",
"zapasach ropy naftowej", "zasadach finansowania nauki oraz niektórych innych ustaw",
"zasadach prowadzenia polityki rozwoju oraz niektórych innych ustaw",
"zasadach prowadzenia zbiórek publicznych", "zasadach realizacji programów w zakresie polityki spójności finansowanych w perspektywie finansowej 2014",
"zawodach pielęgniarki i położnej oraz niektórych innych ustaw",
"zmianie Konstytucji Rzeczypospolitej Polskiej", "zmianie niektórych ustaw w związku z realizacją ustawy budżetowej",
"zmianie niektórych ustaw w związku ze wzmocnieniem narzędzi ochrony krajobrazu",
"zmianie niektórych ustaw związanych z realizacją ustawy budżetowej",
"zmianie ustaw regulujących warunki dostępu do wykonywania niektórych zawodów",
"zmianie ustaw regulujących wykonywanie niektórych zawodów",
"zmianie ustawy budżetowej na rok 2013", "zużytym sprzęcie elektrycznym i elektronicznym",
"związkach partnerskich", "zwrocie podatku akcyzowego zawartego w cenie oleju napędowego wykorzystywanego do produkcji rolnej"
)

names(ustawy) <- paste("ustawa o", ustawy)

ui <- fluidPage(
tags$head(tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
                        ga('create', 'UA-5650686-6', 'auto');
                        ga('send', 'pageview');")),

includeCSS("style.css"),

fluidRow(column(12, HTML("<b>Podobieństwa w głosowaniach posłów VII kadencji</b>.<br>
                       Wybierz ustawy nad którymi głosowano (lub pozostaw puste pole) i naciśnij przycisk <b>Pokaż</b>."))),

fluidRow(column(3, br(),p("Tylko głosowania o ustawach ")),
column(4, selectInput("slowo", "", choices = sort(ustawy), selected = "", multiple = TRUE)),
column(3, br(),actionButton("go", "Pokaż!")),
column(2, selectInput("typ", "", choices = c("fan", "phylogram", "cladogram", "unrooted", "radial"), selected = ""))  ),
fluidRow(column(12, plotOutput("speakerDendro", width = 1300, height = 1300))),
#  fluidRow(column(12, plotOutput("speakerDendro2", width = 1000, height = 300))),
fluidRow(column(12,
HTML("Aplikacja wykonana w ramach hackatonu <a href='http://www.meetup.com/Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup/events/225061731/'>Jak oni głosowali</a> organizowanego przez fundację <a href='http://smarterpoland.pl'>SmarterPoland</a>. <br>
                       Współpraca i konsultacje merytoryczne: <a href='http://mamprawowiedziec.pl/'>Mam Prawo Wiedzieć</a><br>
                       Dane pobrane z pakietu <a href='https://github.com/mi2-warsaw/sejmRP'>sejmRP</a><br>
                       <br><br><a href='http://mamprawowiedziec.pl/'><img width='200px' src='https://github.com/mi2-warsaw/JakOniGlosowali/raw/master/wyszukiwarka/shiny/logo-MPW-CMYK-pion.jpg'/></a>&nbsp;&nbsp;
                       <a href='http://smarterpoland.pl'><img width='140px' src='https://github.com/mi2-warsaw/JakOniGlosowali/raw/master/wyszukiwarka/shiny/smarterpoland.png'/></a>.")
))
)

server <- function(input, output) {

    wartosc <- eventReactive(input$go, {
        c(input$typ, input$slowo)
    })

    output$speakerDendro <- renderPlot({
        withProgress(message = 'Twają obliczenia,',
        detail = 'To może chwilę potrwać...', value = 0, {
            getSpeakerDendro(wartosc())
        })
    })

    output$speakerDendro2 <- renderPlot({
        withProgress(message = 'Twają obliczenia,',
        detail = 'To może chwilę potrwać...', value = 0, {
            getSpeakerDendro2(wartosc())
        })
    })

}

shinyApp(ui = ui, server = server)