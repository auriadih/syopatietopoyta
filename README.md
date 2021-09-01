# syopatietopoyta

![etusivu](https://user-images.githubusercontent.com/89851080/131653331-00077fff-30df-4c0d-aef6-5c70bc338a7b.png)

# Shiny dashboard

Dashboard_data.rds luetaan sisään ja aggregaatit puretaan omiksi datoiksi.
App.R: 
Aggregaateista tehdään kuvaajat (server-osio), jotka luetaan käyttöliittymään (ui-osio). Tietopöytä koostuu neljästä välilehdestä: Tunnusluvut, Hoitojaksot, Kuvantaminen ja Tutkimusnäytteet. Tunnusluvut välilehdellä löytyvät yleiset toimintaluvut viivadiagrammeina ja infolaatikoista viime vuoden lukumäärä sekä kuluvan vuoden lukumäärä. Infolaatikoista löytyy helposti tieto raportointia varten ja viivadiagrammeista voi seurata yleistä trendiä. Muilla välilehdillä on vain viivadiagrammit yleisen trendin seurantaan.
Kuvantamisen välilehdellä samassa kuvaajassa on eri menetelmät viivadiagrammeina. Klikkaamalla värien selitystekstiä voi kuvaajasta poistaa haluamansa menetelmät. Kuvaajan työkalupalkista voi valita lataako kuvaajan png-kuvana omalle koneelle, vertailla datapisteitä kaikissa viivadiagrammeissa samaan aikaan (näyttää saman vuosiluvun datapisteen tiedon kaikissa viivadiagrammeissa yhtä aikaa) tai katsoa vain yhden datapisteen tietoa.
