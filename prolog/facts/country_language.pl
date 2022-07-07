:- module(country_language, [top_country_language/2]).
:- encoding(utf8).

top_country_language(Country, Language):- country_languages(Country, Languages), top_language(Languages, Language).

top_language(Languages, Language):-
  order_by([desc(P)], member(Language/P, Languages)), !;
  member(Language, Languages).

country_languages('Afghanistan',['Dari'/50, 'Pashto'/35, 'Turkic languages'/11]).
country_languages('Albania',['Albanian', 'Greek', 'Vlach', 'Romani', 'Slavic dialects']).
country_languages('Algeria',['Arabic', 'French', 'Berber dialects']).
country_languages('American Samoa',['Samoan'/90.6, 'English'/2.9, 'Tongan'/2.4]).
country_languages('Andorra',['Catalan', 'French', 'Castilian', 'Portuguese']).
country_languages('Angola',['Portuguese', 'Bantu']).
country_languages('Anguilla',['English']).
country_languages('Antigua and Barbuda',['English', 'local dialects']).
country_languages('Argentina',['Spanish', 'Italian', 'English', 'German', 'French']).
country_languages('Armenia',['Armenian'/97.7, 'Yezidi'/1, 'Russian'/0.9]).
country_languages('Aruba',['Papiamento'/66.3, 'Spanish'/12.6, 'English'/7.7, 'Dutch'/5.8]).
country_languages('Australia',['English'/78.5, 'Chinese'/2.5, 'Italian'/1.6, 'Greek'/1.3, 'Arabic'/1.2, 'Vietnamese'/1]).
country_languages('Austria',['German'/88.6, 'Turkish'/2.3, 'Serbian'/2.2, 'Croatian'/1.6]).
country_languages('Azerbaijan',['Azerbaijani'/90.3, 'Lezgi'/2.2, 'Russian'/1.8, 'Armenian'/1.5]).
country_languages('Bahamas, The',['English', 'Creole']).
country_languages('Bahrain',['Arabic', 'English', 'Farsi', 'Urdu']).
country_languages('Bangladesh',['Bangla', 'English']).
country_languages('Barbados',['English']).
country_languages('Belarus',['Belarusian'/36.7, 'Russian'/62.8]).
country_languages('Belgium',['Dutch'/60, 'French'/40]).
country_languages('Belize',['Spanish'/46, 'Creole'/32.9, 'Mayan dialects'/8.9, 'English'/3.9, 'Garifuna'/3.4, 'German'/3.3]).
country_languages('Benin',['French', 'Fon', 'Yoruba']).
country_languages('Bermuda',['English', 'Portuguese']).
country_languages('Bhutan',['Dzongkha', 'Tibetan dialects', 'Nepalese dialects']).
country_languages('Bolivia',['Spanish'/60.7, 'Quechua'/21.2, 'Aymara'/14.6]).
country_languages('Bosnia and Herzegovina',['Bosnian', 'Croatian', 'Serbian']).
country_languages('Botswana',['Setswana'/78.2, 'Kalanga'/7.9, 'Sekgalagadi'/2.8, 'English'/2.1]).
country_languages('Brazil',['Portuguese']).
country_languages('British Virgin Islands',['English']).
country_languages('Brunei',['Malay', 'English', 'Chinese']).
country_languages('Bulgaria',['Bulgarian'/84.5, 'Turkish'/9.6, 'Roma'/4.1]).
country_languages('Burkina Faso',['French']).
country_languages('Burma',['Burmese']).
country_languages('Burundi',['Kirundi', 'French', 'Swahili']).
country_languages('Cambodia',['Khmer'/95, 'French', 'English']).
country_languages('Cameroon',['English', 'French']).
country_languages('Canada',['English'/58.8, 'French'/21.6]).
country_languages('Cape Verde',['Portuguese', 'Crioulo']).
country_languages('Cayman Islands',['English'/95, 'Spanish'/3.2]).
country_languages('Central African Republic',['French', 'Sangho']).
country_languages('Chad',['French', 'Arabic', 'Sara']).
country_languages('Chile',['Spanish', 'Mapudungun', 'German', 'English']).
country_languages('China',['Mandarin', 'Yue', 'Wu', 'Minbei', 'Minnan', 'Xiang', 'Gan', 'Hakka dialects']).
country_languages('Christmas Island',['English', 'Chinese', 'Malay']).
country_languages('Cocos Islands',['Malay', 'English']).
country_languages('Colombia',['Spanish']).
country_languages('Comoros',['Arabic', 'French', 'Shikomoro']).
country_languages('Congo, Democratic Republic of the',['French', 'Lingala', 'Kingwana', 'Kikongo', 'Tshiluba']).
country_languages('Congo, Republic of the',['French', 'Lingala', 'Monokutuba']).
country_languages('Cook Islands',['English', 'Maori']).
country_languages('Costa Rica',['Spanish', 'English']).
country_languages('Cote d\'Ivoire',['French', 'Dioula']).
country_languages('Croatia',['Croatian'/96.1, 'Serbian'/1, 'undesignated'/2.9]).
country_languages('Cuba',['Spanish']).
country_languages('Curacao',['Papiamento'/81.2, 'Dutch'/8, 'Spanish'/4, 'English'/2.9]).
country_languages('Cyprus',['Greek', 'Turkish', 'English']).
country_languages('Czech Republic',['Czech'/94.9, 'Slovak'/2]).
country_languages('Denmark',['Danish', 'Faroese', 'Greenlandic', 'German']).
country_languages('Dhekelia',['English', 'Greek']).
country_languages('Djibouti',['French', 'Arabic', 'Somali', 'Afar']).
country_languages('Dominica',['English', 'French patois']).
country_languages('Dominican Republic',['Spanish']).
country_languages('Ecuador',['Spanish', 'Amerindian languages']).
country_languages('Egypt',['Arabic', 'English', 'French']).
country_languages('El Salvador',['Spanish', 'Nahua']).
country_languages('Equatorial Guinea',['Spanish'/67.6 , 'Fang', 'Bubi']).
country_languages('Eritrea',['Tigrinya', 'Arabic', 'English', 'Tigre', 'Kunama', 'Afar']).
country_languages('Estonia',['Estonian'/67.3, 'Russian'/29.7]).
country_languages('Ethiopia',['Amarigna'/32.7, 'Oromigna'/31.6, 'Tigrigna'/6.1, 'Somaligna'/6, 'Guaragigna'/3.5, 'Sidamigna'/3.5, 'Hadiyigna'/1.7 , 'English', 'Arabic']).
country_languages('Falkland Islands',['English']).
country_languages('Faroe Islands',['Faroese', 'Danish']).
country_languages('Fiji',['English', 'Fijian', 'Hindustani']).
country_languages('Finland',['Finnish'/91.2, 'Swedish'/5.5]).
country_languages('France',['French'/100]).
country_languages('French Polynesia',['French'/61.1, 'Polynesian'/31.4]).
country_languages('Gabon',['French', 'Fang', 'Myene', 'Nzebi', 'Bapounou'/'Eschira', 'Bandjabi']).
country_languages('Gambia, The',['English', 'Mandinka', 'Wolof', 'Fula']).
country_languages('Gaza Strip',['Arabic', 'Hebrew', 'English']).
country_languages('Georgia',['Georgian'/71, 'Russian'/9, 'Armenian'/7, 'Azeri'/6]).
country_languages('Germany',['German']).
country_languages('Ghana',['Asante'/14.8, 'Ewe'/12.7, 'Fante'/9.9, 'Boron'/4.6, 'Dagomba'/4.3, 'Dangme'/4.3, 'Dagarte'/3.7, 'Akyem'/3.4, 'Ga'/3.4, 'Akuapem'/2.9]).
country_languages('Gibraltar',['English', 'Spanish', 'Italian', 'Portuguese']).
country_languages('Greece',['Greek'/99]).
country_languages('Greenland',['Greenlandic', 'Danish', 'English']).
country_languages('Grenada',['English', 'French patois']).
country_languages('Guam',['English'/38.3, 'Chamorro'/22.2, 'Philippine languages'/22.2]).
country_languages('Guatemala',['Spanish'/60, 'Amerindian languages'/40]).
country_languages('Guernsey',['English', 'French', 'Norman-French']).
country_languages('Guinea',['French']).
country_languages('Guinea-Bissau',['Portuguese', 'Crioulo', 'African languages']).
country_languages('Guyana',['English', 'Amerindian dialects', 'Creole', 'Caribbean Hindustani', 'Urdu']).
country_languages('Haiti',['French', 'Creole']).
country_languages('Holy See',['Italian', 'Latin', 'French']).
country_languages('Honduras',['Spanish', 'Amerindian dialects']).
country_languages('Hong Kong',['Cantonese'/90.8, 'English'/2.8, 'Putonghua'/0.9]).
country_languages('Hungary',['Hungarian'/93.6]).
country_languages('Iceland',['Icelandic', 'English', 'Nordic languages', 'German']).
country_languages('India',['Hindi'/41, 'Bengali'/8.1, 'Telugu'/7.2, 'Marathi'/7, 'Tamil'/5.9, 'Urdu'/5, 'Gujarati'/4.5, 'Kannada'/3.7, 'Malayalam'/3.2, 'Oriya'/3.2, 'Punjabi'/2.8, 'Assamese'/1.3, 'Maithili'/1.2]).
country_languages('Indonesia',['Bahasa Indonesia', 'English', 'Dutch']).
country_languages('Iran',['Persian'/58, 'Turkic'/26, 'Kurdish'/9, 'Luri'/2, 'Balochi'/1, 'Arabic'/1, 'Turkish'/1]).
country_languages('Iraq',['Arabic', 'Kurdish', 'Turkoman', 'Assyrian', 'Armenian']).
country_languages('Ireland',['English', 'Irish']).
country_languages('Isle of Man',['English', 'Manx Gaelic']).
country_languages('Israel',['Hebrew', 'Arabic', 'English']).
country_languages('Italy',['Italian', 'German', 'French', 'Slovene']).
country_languages('Jamaica',['English', 'English patois']).
country_languages('Japan',['Japanese']).
country_languages('Jersey',['English'/94.5, 'Portuguese'/4.6]).
country_languages('Jordan',['Arabic', 'English']).
country_languages('Kazakhstan',['Kazakh'/64.4, 'Russian'/95]).
country_languages('Kenya',['English', 'Swahili']).
country_languages('Kiribati',['I-Kiribati', 'English']).
country_languages('Korea, North',['Korean']).
country_languages('Korea, South',['Korean', 'English']).
country_languages('Kosovo',['Albanian', 'Serbian', 'Bosnian', 'Turkish', 'Roma']).
country_languages('Kuwait',['Arabic', 'English']).
country_languages('Kyrgyzstan',['Kyrgyz'/64.7, 'Uzbek'/13.6, 'Russian'/12.5, 'Dungun'/1]).
country_languages('Laos',['Lao', 'French', 'English']).
country_languages('Latvia',['Latvian'/58.2, 'Russian'/37.5, 'Lithuanian'/4.3]).
country_languages('Lebanon',['Arabic', 'French', 'English', 'Armenian']).
country_languages('Lesotho',['Sesotho', 'English', 'Zulu', 'Xhosa']).
country_languages('Liberia',['English'/20]).
country_languages('Libya',['Arabic', 'Italian', 'English']).
country_languages('Liechtenstein',['German', 'Alemannic dialect']).
country_languages('Lithuania',['Lithuanian'/82, 'Russian'/8, 'Polish'/5.6]).
country_languages('Luxembourg',['Luxembourgish', 'German', 'French']).
country_languages('Macau',['Cantonese'/85.7, 'Hokkien'/4, 'Mandarin'/3.2, 'English'/1.5, 'Tagalog'/1.3]).
country_languages('Macedonia',['Macedonian'/66.5, 'Albanian'/25.1, 'Turkish'/3.5, 'Roma'/1.9, 'Serbian'/1.2]).
country_languages('Madagascar',['French', 'Malagasy', 'English']).
country_languages('Malawi',['Chichewa'/57.2, 'Chinyanja'/12.8, 'Chiyao'/10.1, 'Chitumbuka'/9.5, 'Chisena'/2.7, 'Chilomwe'/2.4, 'Chitonga'/1.7]).
country_languages('Malaysia',['Bahasa Malaysia', 'English', 'Chinese', 'Tamil', 'Telugu', 'Malayalam', 'Panjabi', 'Thai']).
country_languages('Maldives',['Maldivian Dhivehi', 'English']).
country_languages('Mali',['French', 'Bambara'/80]).
country_languages('Malta',['Maltese'/90.2, 'English'/6, 'multilingual'/3]).
country_languages('Marshall Islands',['Marshallese'/98.2]).
country_languages('Mauritania',['Arabic', 'Pulaar', 'Soninke', 'Wolof', 'French', 'Hassaniya']).
country_languages('Mauritius',['Creole'/80.5, 'Bhojpuri'/12.1, 'French'/3.4, 'English']).
country_languages('Mayotte',['Mahorian', 'French'/35]).
country_languages('Mexico',['Spanish']).
country_languages('Micronesia, Federated States of',['English', 'Chuukese', 'Kosrean', 'Pohnpeian', 'Yapese', 'Ulithian', 'Woleaian', 'Nukuoro', 'Kapingamarangi']).
country_languages('Moldova',['Moldovan', 'Russian', 'Gagauz']).
country_languages('Monaco',['French', 'English', 'Italian', 'Monegasque']).
country_languages('Mongolia',['Khalkha Mongol'/90, 'Turkic', 'Russian']).
country_languages('Montenegro',['Serbian'/63.6, 'Montenegrin'/22, 'Bosnian'/5.5, 'Albanian'/5.3, 'unspecified'/3.7]).
country_languages('Montserrat',['English']).
country_languages('Morocco',['Arabic', 'Berber dialects', 'French']).
country_languages('Mozambique',['Emakhuwa'/26.1, 'Xichangana'/11.3, 'Portuguese'/8.8, 'Elomwe'/7.6, 'Cisena'/6.8, 'Echuwabo'/5.8, 'unspecified'/1.3]).
country_languages('Namibia',['English'/7, 'Afrikaans'/60, 'German'/32]).
country_languages('Nauru',['Nauruan', 'English']).
country_languages('Nepal',['Nepali'/47.8, 'Maithali'/12.1, 'Bhojpuri'/7.4, 'Tharu'/5.8, 'Tamang'/5.1, 'Newar'/3.6, 'Magar'/3.3, 'Awadhi'/2.4]).
country_languages('Netherlands',['Dutch', 'Frisian']).
country_languages('Netherlands Antilles',['Papiamento'/65.4, 'English'/15.9, 'Dutch'/7.3, 'Spanish'/6.1, 'Creole'/1.6]).
country_languages('New Caledonia',['French'/33, 'Melanesian-Polynesian dialects']).
country_languages('New Zealand',['English'/91.2, 'Maori'/3.9, 'Samoan'/2.1, 'French'/1.3, 'Hindi'/1.1, 'Yue'/1.1, 'Northern Chinese'/1 , 'New Zealand Sign Language']).
country_languages('Nicaragua',['Spanish'/97.5, 'Miskito'/1.7]).
country_languages('Niger',['French', 'Hausa', 'Djerma']).
country_languages('Nigeria',['English', 'Hausa', 'Yoruba', 'Igbo', 'Fulani']).
country_languages('Niue',['English', 'Niuean', 'English']).
country_languages('Norfolk Island',['English', 'Norfolk']).
country_languages('Northern Mariana Islands',['Philippine languages'/24.4, 'Chinese'/23.4, 'Chamorro'/22.4, 'English'/10.8, 'other'/9.6]).
country_languages('Norway',['Bokmal Norwegian', 'Nynorsk Norwegian', 'small Sami']).
country_languages('Oman',['Arabic', 'English', 'Baluchi', 'Urdu', 'Indian dialects']).
country_languages('Pakistan',['Punjabi'/48, 'Sindhi'/12, 'Siraiki'/10, 'Pashtu'/8, 'Urdu'/8, 'Balochi'/3, 'Hindko'/2, 'Brahui'/1, 'English', 'Burushaski']).
country_languages('Palau',['Palauan'/64.7, 'Tobi', 'Angaur', 'Filipino'/13.5, 'English'/9.4, 'Chinese'/5.7, 'Carolinian'/1.5, 'Japanese'/1.5]).
country_languages('Panama',['Spanish', 'English']).
country_languages('Papua New Guinea',['Tok Pisin', 'English', 'Hiri Motu']).
country_languages('Paraguay',['Spanish', 'Guarani']).
country_languages('Peru',['Spanish'/84.1, 'Quechua'/13, 'Aymara'/1.7, 'Ashaninka'/0.3, 'other'/0.2]).
country_languages('Philippines',['Filipino', 'English', 'Tagalog', 'Cebuano', 'Ilocano', 'Hiligaynon', 'Bicol', 'Waray', 'Pampango', 'Pangasinan']).
country_languages('Pitcairn Islands',['English', 'Pitkern']).
country_languages('Poland',['Polish'/97.8]).
country_languages('Portugal',['Portuguese', 'Mirandese']).
country_languages('Puerto Rico',['Spanish', 'English']).
country_languages('Qatar',['Arabic', 'English']).
country_languages('Romania',['Romanian'/91, 'Hungarian'/6.7, 'Romany'/1.1]).
country_languages('Russia',['Russian']).
country_languages('Rwanda',['Kinyarwanda', 'Bantu', 'French', 'English', 'Swahili']).
country_languages('Saint Barthelemy',['French', 'English']).
country_languages('Saint Helena, Ascension, and Tristan da Cunha',['English']).
country_languages('Saint Kitts and Nevis',['English']).
country_languages('Saint Lucia',['English', 'French patois']).
country_languages('Saint Martin',['French', 'English', 'Dutch', 'French Patois', 'Spanish', 'Papiamento']).
country_languages('Saint Pierre and Miquelon',['French']).
country_languages('Saint Vincent and the Grenadines',['English', 'French patois']).
country_languages('Samoa',['Samoan', 'English']).
country_languages('San Marino',['Italian']).
country_languages('Sao Tome and Principe',['Portuguese']).
country_languages('Saudi Arabia',['Arabic']).
country_languages('Senegal',['French', 'Wolof', 'Pulaar', 'Jola', 'Mandinka']).
country_languages('Serbia',['Serbian'/88.3, 'Hungarian'/3.8, 'Bosniak'/1.8, 'Romany'/1.1]).
country_languages('Seychelles',['Creole'/91.8, 'English'/4.9]).
country_languages('Sierra Leone',['English', 'Mende', 'Temne', 'Krio']).
country_languages('Singapore',['Mandarin'/35, 'English'/23, 'Malay'/14.1, 'Hokkien'/11.4, 'Cantonese'/5.7, 'Teochew'/4.9, 'Tamil'/3.2]).
country_languages('Sint Maarten',['English'/67.5, 'Spanish'/12.9, 'Creole'/8.2, 'Dutch'/4.2, 'Papiamento'/2.2, 'French'/1.5]).
country_languages('Slovakia',['Slovak'/83.9, 'Hungarian'/10.7, 'Roma'/1.8, 'Ukrainian'/1]).
country_languages('Slovenia',['Slovenian'/91.1, 'Serbo-Croatian'/4.5]).
country_languages('Solomon Islands',['Melanesian pidgin', 'English']).
country_languages('Somalia',['Somali', 'Arabic', 'Italian', 'English']).
country_languages('South Africa',['IsiZulu'/23.8, 'IsiXhosa'/17.6, 'Afrikaans'/13.3, 'Sepedi'/9.4, 'English'/8.2, 'Setswana'/8.2, 'Sesotho'/7.9, 'Xitsonga'/4.4 , 'isiNdebele', 'Tshivenda', 'siSwati']).
country_languages('Spain',['Castilian Spanish'/74, 'Catalan'/17, 'Galician'/7, 'Basque'/2]).
country_languages('Sri Lanka',['Sinhala'/74, 'Tamil'/18]).
country_languages('Sudan',['Arabic', 'English', 'Nubian', 'Ta Bedawie', 'diverse dialects of Nilotic', 'Nilo-Hamitic', 'Sudanic languages']).
country_languages('Suriname',['Dutch', 'English', 'Sranang Tongo', 'Caribbean Hindustani', 'Javanese']).
country_languages('Svalbard',['Norwegian', 'Russian']).
country_languages('Swaziland',['English', 'siSwati']).
country_languages('Sweden',['Swedish', 'Sami', 'Finnish']).
country_languages('Switzerland',['German'/63.7, 'French'/20.4, 'Italian'/6.5, 'Serbo-Croatian'/1.5, 'Albanian'/1.3, 'Portuguese'/1.2, 'Spanish'/1.1, 'English'/1, 'Romansch'/0.5]).
country_languages('Syria',['Arabic', 'Kurdish', 'Armenian', 'Aramaic', 'Circassian', 'French']).
country_languages('Taiwan',['Mandarin Chinese', 'Taiwanese', 'Hakka dialects']).
country_languages('Tajikistan',['Tajik', 'Russian']).
country_languages('Tanzania',['Swahili', 'Kiunguja', 'English', 'Arabic']).
country_languages('Thailand',['Thai', 'English']).
country_languages('Timor-Leste',['Tetum', 'Portuguese', 'Indonesian', 'English']).
country_languages('Togo',['French', 'Ewe', 'Mina', 'Kabye', 'Dagomba']).
country_languages('Tokelau',['Tokelauan', 'English']).
country_languages('Tonga',['Tongan', 'English']).
country_languages('Trinidad and Tobago',['English', 'Caribbean Hindustani', 'French', 'Spanish', 'Chinese']).
country_languages('Tunisia',['Arabic', 'French']).
country_languages('Turkey',['Turkish', 'Kurdish']).
country_languages('Turkmenistan',['Turkmen'/72, 'Russian'/12, 'Uzbek'/9]).
country_languages('Turks and Caicos Islands',['English']).
country_languages('Tuvalu',['Tuvaluan', 'English', 'Samoan', 'Kiribati']).
country_languages('Uganda',['English', 'Ganda', 'Swahili', 'Arabic']).
country_languages('Ukraine',['Ukrainian'/67, 'Russian'/24]).
country_languages('United Arab Emirates',['Arabic', 'Persian', 'English', 'Hindi', 'Urdu']).
country_languages('United Kingdom',['English']).
country_languages('United States',['English'/82.1, 'Spanish'/10.7]).
country_languages('Uruguay',['Spanish', 'Portunol', 'Brazilero']).
country_languages('Uzbekistan',['Uzbek'/74.3, 'Russian'/14.2, 'Tajik'/4.4]).
country_languages('Vanuatu',['local languages'/72.6, 'pidgin'/23.1, 'English'/1.9, 'French'/1.4]).
country_languages('Venezuela',['Spanish']).
country_languages('Vietnam',['Vietnamese', 'English', 'some French', 'Chinese', 'Khmer']).
country_languages('Virgin Islands',['English'/74.7, 'Spanish'/16.8, 'French'/6.6]).
country_languages('Wallis and Futuna',['Wallisian'/58.9, 'Futunian'/30.1, 'French'/10.8]).
country_languages('West Bank',['Arabic', 'Hebrew', 'English']).
country_languages('Western Sahara',['Hassaniya Arabic', 'Moroccan Arabic']).
country_languages('Yemen',['Arabic']).
country_languages('Zambia',['Bemba'/30.1, 'Nyanja'/10.7, 'Tonga'/10.6, 'Lozi'/5.7, 'Chewa'/4.9, 'Nsenga'/3.4, 'Tumbuka'/2.5, 'Lunda'/2.2, 'Kaonde'/2, 'Lala'/2, 'Luvale'/1.7, 'English'/1.7]).
country_languages('Zimbabwe',['English', 'Shona', 'Sindebele']).
