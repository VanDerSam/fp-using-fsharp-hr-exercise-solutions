type Name = string
type TelephoneNumber = string
type Sex = Male | Female
type YearOfBirthday = int

type ThemeOfInterest = string
type ThemesOfInterest = Set<ThemeOfInterest>

type Client = {name: Name; 
               phone: TelephoneNumber;
               sex: Sex;
               year: YearOfBirthday;
               themes: ThemesOfInterest}

type File = Client list

type SearchRequest = {sexCriteria:Sex;
                      yearCriteria:YearOfBirthday;
                      themesCriteria:ThemesOfInterest}

let areOverlapped set1 set2 = 
    let intersetedSet = Set.intersect set1 set2 in
    intersetedSet <> Set.empty

// Tests
areOverlapped (Set.ofList []) (Set.ofList [])
areOverlapped (Set.ofList []) (Set.ofList [1])
areOverlapped (Set.ofList [1]) (Set.ofList [])
areOverlapped (Set.ofList [1]) (Set.ofList [1])
areOverlapped (Set.ofList [1]) (Set.ofList [2])
areOverlapped (Set.ofList [1]) (Set.ofList [1;2])
areOverlapped (Set.ofList [1;3]) (Set.ofList [3;1])

let findClients searchRequest file = 
    let isSatisfied (client:Client) = 
        client.sex <> searchRequest.sexCriteria 
        && abs (client.year - searchRequest.yearCriteria) < 10 
        && areOverlapped client.themes searchRequest.themesCriteria
    let findByFile file = List.fold (fun result fileItem -> if isSatisfied fileItem then fileItem::result else result) [] file
    findByFile file

let datingBureauFile = [{name="Alex M"; phone="123"; sex=Male; year=1990; themes=set ["cinema";"sport"]};
                        {name="Sergey F"; phone="456"; sex=Male; year=1988; themes=set ["traveling";"cars"]};
                        {name="Dmitry G"; phone="4567"; sex=Male; year=1977; themes=set ["sport";"bikes"]};
                        {name="Katy P"; phone="789"; sex=Female; year=1978; themes=set ["coocking";"traveling"]};
                        {name="Nastya K"; phone="012"; sex=Female; year=1992; themes=set ["coocking";"languages"]};
                        {name="Olya L"; phone="0130"; sex=Female; year=1995; themes=set ["painting";"traveling";"bikes"]}]

findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=set ["sport";"computers"]} datingBureauFile
findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=set ["sport";"traveling"]} datingBureauFile
findClients {sexCriteria=Male; yearCriteria=1985; themesCriteria=set ["languages"]} datingBureauFile
