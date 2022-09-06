:- module(country_date_endianness, [top_endianness/2]).
:- encoding(utf8).

top_endianness(Country, Endianness):- country_date_endianness_population(Country, Cases, _), member(Endianness, Cases).

country_date_endianness_population('Afghanistan', [little,big], 32).
country_date_endianness_population('Albania', [little,big], 2.8).
country_date_endianness_population('Algeria', [little], 43).
country_date_endianness_population('Argentina', [little], 45).
country_date_endianness_population('Australia', [little], 25).
country_date_endianness_population('Austria', [little], 9).
country_date_endianness_population('Bangladesh', [little], 166).
country_date_endianness_population('Belgium', [little], 12).
country_date_endianness_population('Bhutan', [big], 0.74).
country_date_endianness_population('Brazil', [little], 210).
country_date_endianness_population('Cambodia', [little], 16).
country_date_endianness_population('Cameroon', [little,big], 24).
country_date_endianness_population('Canada', [middle,big,little], 38).
country_date_endianness_population('Cayman Islands', [little,middle], 0.63).
country_date_endianness_population('China', [big], 1398).
country_date_endianness_population('Colombia', [little], 51).
country_date_endianness_population('Congo, Democratic Republic of the', [little], 87).
country_date_endianness_population('Croatia', [little], 4).
country_date_endianness_population('Czech Republic', [little], 11).
country_date_endianness_population('Denmark', [little], 5.8).
country_date_endianness_population('Egypt', [little], 99).
country_date_endianness_population('Estonia', [little], 1.3).
country_date_endianness_population('Ethiopia', [little], 99).
country_date_endianness_population('Finland', [little], 5.5).
country_date_endianness_population('France', [little,big], 66).
country_date_endianness_population('Germany', [little,big], 83).
country_date_endianness_population('Ghana', [middle,big,little], 30).
country_date_endianness_population('Greenland', [little,middle], 0.056).
country_date_endianness_population('Guatemala', [little], 18).
country_date_endianness_population('Honduras', [little], 9.2).
country_date_endianness_population('Hungary', [big], 10).
country_date_endianness_population('India', [little,big], 1366).
country_date_endianness_population('Indonesia', [little], 268).
country_date_endianness_population('Iran', [little,big], 82).
country_date_endianness_population('Iraq', [little], 40).
country_date_endianness_population('Ireland', [little], 5).
country_date_endianness_population('Italy', [little], 60).
country_date_endianness_population('Japan', [big], 126).
country_date_endianness_population('Kenya', [middle,big,little], 52).
country_date_endianness_population('Korea, North', [big], 25).
country_date_endianness_population('Korea, South', [big], 52).
country_date_endianness_population('Kyrgyzstan', [little], 6.4).
country_date_endianness_population('Latvia', [little], 1.9).
country_date_endianness_population('Lithuania', [big], 2.8).
country_date_endianness_population('Malaysia', [little,middle], 33).
country_date_endianness_population('Mexico', [little], 127).
country_date_endianness_population('Mongolia', [big], 3.3).
country_date_endianness_population('Morocco', [little], 35).
country_date_endianness_population('Myanmar', [little,big], 54).
country_date_endianness_population('Nepal', [little,big], 30).
country_date_endianness_population('Netherlands', [little], 17).
country_date_endianness_population('New Zealand', [little], 5.0).
country_date_endianness_population('Nigeria', [little], 193).
country_date_endianness_population('Norway', [little], 5.4).
country_date_endianness_population('Pakistan', [little], 212).
country_date_endianness_population('Panama', [little,middle], 4.2).
country_date_endianness_population('Papua New Guinea', [little], 8.6).
country_date_endianness_population('Peru', [little], 32).
country_date_endianness_population('Philippines', [little,middle], 107).
country_date_endianness_population('Poland', [little,big], 38).
country_date_endianness_population('Puerto Rico', [little,middle], 3.2).
country_date_endianness_population('Romania', [little], 19).
country_date_endianness_population('Russia', [little,big], 147).
country_date_endianness_population('Saudi Arabia', [little], 33).
country_date_endianness_population('Slovakia', [little], 5.5).
country_date_endianness_population('Slovenia', [little], 2.1).
country_date_endianness_population('Somalia', [little,middle], 16).
country_date_endianness_population('South Africa', [middle,big,little], 60).
country_date_endianness_population('Spain', [little,big], 47).
country_date_endianness_population('Sri Lanka', [little,big], 22).
country_date_endianness_population('Sudan', [little], 41).
country_date_endianness_population('Sweden', [big,little], 10.4).
country_date_endianness_population('Switzerland', [little], 8.7).
country_date_endianness_population('Taiwan', [big], 24).
country_date_endianness_population('Tajikistan', [little], 8.9).
country_date_endianness_population('Tanzania', [little], 56).
country_date_endianness_population('Thailand', [little], 66).
country_date_endianness_population('Togo', [little,middle], 7.5).
country_date_endianness_population('Tunisia', [little], 12).
country_date_endianness_population('Turkey', [little], 82).
country_date_endianness_population('Turkmenistan', [little], 5.9).
country_date_endianness_population('Uganda', [little], 40).
country_date_endianness_population('Ukraine', [little], 42).
country_date_endianness_population('United Kingdom', [little,big], 66).
country_date_endianness_population('United States', [middle,big], 328).
country_date_endianness_population('Uzbekistan', [little,big], 33).
country_date_endianness_population('Venezuela', [little], 32).
country_date_endianness_population('Vietnam', [little,big], 95).
country_date_endianness_population('Yemen', [little], 30).
