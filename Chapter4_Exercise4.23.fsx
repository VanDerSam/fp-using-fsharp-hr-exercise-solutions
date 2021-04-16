type Name = string
type TelephoneNumber = string
type Sex = Male | Female
type YearOfBirthday = int
type ThemeOfInterest = string
type ThemesOfInterest = ThemeOfInterest list

type Client = {name: Name; 
               phone: TelephoneNumber;
               sex: Sex;
               year: YearOfBirthday;
               themes: ThemesOfInterest}

type File = Client list

type SearchRequest = {sexCriteria:Sex;
                      yearCriteria:YearOfBirthday;
                      themesCriteria:ThemesOfInterest}

let rec elemIsInList elem = function
    | [] -> false
    | x::xs -> if x = elem then true else elemIsInList elem xs

elemIsInList 5 [1;2;3]
elemIsInList 5 [1;2;3;5;10]
elemIsInList 5 [5;1]
elemIsInList 5 [1;5]
elemIsInList 5 [5]
elemIsInList 5 [1]

let rec areOverlapped list1 list2 = 
    match list1 with
    | [] -> false
    | x::xs -> if elemIsInList x list2 then true else areOverlapped xs list2

areOverlapped [] []
areOverlapped [] [1]
areOverlapped [1] []
areOverlapped [1] [1]
areOverlapped [1] [2]
areOverlapped [1] [1;2]
areOverlapped [1;3] [3;1]

let findClients searchRequest file = 
    let isSatisfied (client:Client) = 
        client.sex <> searchRequest.sexCriteria 
        && abs (client.year - searchRequest.yearCriteria) < 10 
        && areOverlapped client.themes searchRequest.themesCriteria
    let rec findByFile = function
        | [] -> []
        | x::xs -> if isSatisfied x then x::(findByFile xs) else findByFile xs
    findByFile file

let datingBureauFile = [{name="Alex M"; phone="123"; sex=Male; year=1990; themes=["cinema";"sport"]};
                        {name="Sergey F"; phone="456"; sex=Male; year=1988; themes=["traveling";"cars"]};
                        {name="Dmitry G"; phone="4567"; sex=Male; year=1977; themes=["sport";"bikes"]};
                        {name="Katy P"; phone="789"; sex=Female; year=1978; themes=["coocking";"traveling"]};
                        {name="Nastya K"; phone="012"; sex=Female; year=1992; themes=["coocking";"languages"]};
                        {name="Olya L"; phone="0130"; sex=Female; year=1995; themes=["painting";"traveling";"bikes"]}]

findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=["sport";"computers"]} datingBureauFile
findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=["sport";"traveling"]} datingBureauFile
findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=["languages"]} datingBureauFile
