# custom color ramp generator
# philip kraaijenbrink


#' Load preset color ramps and palettes
#'
#' Load color ramps and palettes from presets. Identical to \code{pkrf::pal() besides some argument defaults.}
#' @param name Name of the color ramp (string).
#' @param number Number of output colors desired (integer). Defaults to number of colors in the pallete for qualitative type and to 100 for sequential and diverging types.
#' @param reversed Reverse the output color order (logical).
#' @param random Randomize output color order (logical).
#' @param center Relative position of the center of the color ramp (numeric, 0-1). Useful to change centerpoint of diverging scales.
#' @param show Show a plot of all available color ramps (logical).
#' @return Vector with hex colors strings. 
#' @export
ramp <- function(name='parula', number=NULL, reversed=F, random=F, center=0.5, show=F){
  
  coldat <- tibble::tribble(
    ~name, ~type, ~source, ~colors,
    
    # sequential ramps
    "Jet", 'Sequential', 'MATLAB', c("0000FF","0080FF","00FFFF","80FF80","FFFF00","FF8000","FF0000","800000"),
    "Parula", 'Sequential', 'MATLAB', c("352A87","0567DF","108ED0","1BAEB4","6BBC85","C4BB5E","F7CB33","F9FB0E"),
    "Viridis", 'Sequential', 'Matplotlib', c("440154","482878","3E4A89","31688E","26828E","1F9E89","35B779","6DCD59","B4DE2C","FDE725"),
    "Inferno", 'Sequential', 'Matplotlib', c("000004","1B0C42","4B0C6B","781C6D","A52C60","CF4446","ED6925","FB9A06","F7D03C","FCFFA4"),
    "Magma", 'Sequential', 'Matplotlib', c("000004","180F3E","451077","721F81","9F2F7F","CD4071","F1605D","FD9567","FEC98D","FCFDBF"),
    "Plasma", 'Sequential', 'Matplotlib', c("0D0887","47039F","7301A8","9C179E","BD3786","D8576B","ED7953","FA9E3B","FDC926","F0F921"),
    "Cividis", 'Sequential', 'Matplotlib', c("00204D","00336F","39486B","575C6D","707173","8A8779","A69D75","C4B56C","E4CF5B","FFEA46") ,
    "ElevAG", 'Sequential', 'ArcGIS', c("AFF0E9","FFFFB3","008040","FCBA03","800000","69300D","ABABAB","FFFCFF"),
    "WtSpec", 'Sequential', 'Custom', c("FFFFFF","B7E8FF","ACD88C","DDE241","E83535","380000"),
    "ElevNat1", 'Sequential', 'Custom', c('566314','9ba864','c5cc90','a59a7f','5b4242'),
    "ElevNat2", 'Sequential', 'Custom', c('75a05b','e5d9a7','fcc575','baa395','e2e2e2'),
    "Taupe", 'Sequential', 'Custom', c('22223b','4a4e69','9a8c98','c9ada7','f2e9e4'),
    "Blues", 'Sequential', 'ColorBrewer', c('f7fbff','deebf7','c6dbef','9ecae1','6baed6','4292c6','2171b5','08519c','08306b'),
    "Greens", 'Sequential', 'ColorBrewer', c('f7fcf5','e5f5e0','c7e9c0','a1d99b','74c476','41ab5d','238b45','006d2c','00441b'),
    "BW", 'Sequential', 'ColorBrewer', c('000000','ffffff'),
    "BluesMH",  'Sequential', 'Custom', c('ffffff','232365'),
    "MHramp", 'Sequential', 'Custom', c('FFCD00','c1ae5e','6b6b8c','232365'),
    "Temperature", 'Sequential', 'CPT-City', c("#1316B4","#2331C7","#2D42C9","#3755CB","#365FC6","#466FCF","#507DD2","#598DD6","#629BD9","#7EB9E9","#A5D7FF","#C4E5B7","#B4DFA8","#B0D793","#C7CF74","#DBC85B","#DEBD50","#D9A449","#D39242","#D1853E","#CC7139","#CA6232","#C74528"),
    "GlobWarm",'Sequential', 'CPT-City',c("#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#19151A","#19151A","#302433","#302433","#452A4D","#452A4D","#572963","#572963","#69207D","#69207D","#781294","#781294","#810CA8","#810CA8","#731EA8","#731EA8","#662CA8","#662CA8","#5934A8","#5934A8","#493DA8","#493DA8","#3843A8","#3843A8","#2049A8","#2049A8","#0550AB","#0550AB","#0B5EB8","#0B5EB8","#0E6FC4","#0E6FC4","#0F80D1","#0F80D1","#1092DE","#1092DE","#0CA4EB","#0CA4EB","#07B7F7","#07B7F7","#1DC2F0","#1DC2F0","#32BDC9","#32BDC9","#3FBAA6","#3FBAA6","#43B582","#43B582","#44B361","#44B361","#42AD42","#42AD42","#3CAB1D","#3CAB1D","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#38A800","#43B000","#43B000","#51BA00","#51BA00","#61C900","#61C900","#71D400","#71D400","#84E300","#84E300","#92ED00","#92ED00","#A8FC00","#A8FC00","#B5F700","#B5F700","#C3EB00","#C3EB00","#CFDE00","#CFDE00","#DED300","#DED300","#E8C500","#E8C500","#F2BA00","#F2BA00","#FCAD00","#FCAD00","#FA9200","#FA9200","#F77800","#F77800","#F25D00","#F25D00","#F04400","#F04400","#EB2B00","#EB2B00","#E81700","#E81700","#E60000","#E60000"),
    "Desertification",'Sequential', 'CPT-City',c("#CEED9D","#CEED9D","#D1ED9D","#D1ED9D","#D3ED9A","#D3ED9A","#D7ED9A","#D7ED9A","#D9ED98","#D9ED98","#DCED98","#DCED98","#DFED95","#DFED95","#E1ED95","#E1ED95","#E4ED93","#E4ED93","#E7ED93","#E7ED93","#EAED91","#EAED91","#F0EE92","#F0EE92","#F0ED90","#F0ED90","#F0E990","#F0E990","#F0E48D","#F0E48D","#F0E18B","#F0E18B","#F0DD8B","#F0DD8B","#F0D989","#F0D989","#F0D689","#F0D689","#F0D286","#F0D286","#F0CE86","#F0CE86","#F0CB86","#F0CB86","#EDC785","#EDC785","#EBC383","#EBC383","#E8BE84","#E8BE84","#E3B981","#E3B981","#E0B482","#E0B482","#DEB181","#DEB181","#D9AA7E","#D9AA7E","#D6A77E","#D6A77E","#D4A47D","#D4A47D","#D19F7B","#D19F7B","#CC9B7A","#CC9B7A","#C99679","#C99679","#C79479","#C79479","#C49178","#C49178","#BF8B75","#BF8B75","#BD8975","#BD8975","#BA8573","#BA8573","#B58272","#B58272","#B37F70","#B37F70","#B07B6F","#B07B6F","#AD766C","#AD766C","#AD7266","#AD7266","#AD6F61","#AD6F61","#AD6B5C","#AD6B5C","#AD6857","#AD6857","#AD6451","#AD6451","#AD614C","#AD614C","#AB5C46","#AB5C46","#AB583F","#AB583F","#AB563C","#AB563C","#AB5437","#AB5437","#AB5032","#AB5032","#AB4D2B","#AB4D2B","#AB4926","#AB4926","#A84720","#A84720","#A8431B","#A8431B","#A83F16","#A83F16","#A83E11","#A83E11","#A83B0C","#A83B0C","#A83A07","#A83A07","#A83800","#A83800"),
    "ElevTOPO", 'Sequential', 'CPT-City', c("#71ABD8","#79B2DE","#84B9E3","#8DC1EA","#96C9F0","#A1D2F7","#ACDBFB","#B9E3FF","#C6ECFF","#D8F2FE","#ACD0A5","#94BF8B","#A8C68F","#BDCC96","#D1D7AB","#E1E4B5","#EFEBC0","#E8E1B6","#DED6A3","#D3CA9D","#CAB982","#C3A76B","#B9985A","#AA8753","#AC9A7C","#BAAE9A","#CAC3B8","#E0DED8","#F5F4F2"),
    "Precip1", 'Sequential', 'CPT-City',c("#E5B42C","#F2B464","#F3E977","#91CE7E","#43BE87","#34B485","#069B42"),
    "Precip2", 'Sequential', 'CPT-City',c("#FFFFFF","#EDFAC2","#CDFFCD","#99F0B2","#53BD9F","#32A696","#3296B4","#0570B0","#05508C","#0A1F96","#2C0246","#6A2C5A"),
    "BluesIce",'Sequential', 'CPT-City', c("#EAFDFD","#E8FCFC","#E7FBFB","#E5FAFB","#E3F9FA","#E1F7F9","#E0F6F8","#DEF5F7","#DCF4F6","#DAF3F5","#D8F2F4","#D6F1F3","#D5F0F2","#D3EFF1","#D1EEF0","#CFEDEF","#CDECEF","#CBEBEE","#C9EAED","#C8E9EC","#C6E8EB","#C4E7EA","#C2E6E9","#C0E6E8","#BEE5E7","#BCE4E7","#BAE3E6","#B9E2E5","#B7E1E4","#B5E0E3","#B3DFE3","#B1DEE2","#AFDDE1","#ADDCE0","#ABDBE0","#A9DADF","#A8D9DE","#A6D8DE","#A4D7DD","#A2D6DC","#A0D6DC","#9ED5DB","#9CD4DA","#9AD3DA","#99D2D9","#97D1D9","#95D0D8","#93CFD8","#92CED7","#90CDD7","#8ECCD6","#8CCBD6","#8BCAD5","#89C9D5","#88C8D4","#86C7D4","#85C6D3","#83C5D3","#82C4D3","#80C3D2","#7FC2D2","#7DC1D1","#7CC0D1","#7BBFD0","#79BED0","#78BDD0","#77BCCF","#75BBCF","#74BACE","#73B9CE","#72B8CE","#71B6CD","#6FB5CD","#6EB4CC","#6DB3CC","#6CB2CB","#6BB1CB","#6AB0CB","#69AFCA","#68AECA","#67ADC9","#65ACC9","#64ABC9","#63AAC8","#62A9C8","#61A8C7","#60A7C7","#5FA6C7","#5FA4C6","#5EA3C6","#5DA2C5","#5CA1C5","#5BA0C5","#5A9FC4","#599EC4","#589DC3","#579CC3","#569BC3","#559AC2","#5599C2","#5498C2","#5397C1","#5296C1","#5195C0","#5194C0","#5092C0","#4F91BF","#4E90BF","#4E8FBF","#4D8EBE","#4C8DBE","#4B8CBD","#4B8BBD","#4A8ABD","#4989BC","#4988BC","#4887BC","#4786BB","#4785BB","#4684BB","#4682BA","#4581BA","#4580B9","#447FB9","#447EB9","#437DB8","#437CB8","#427BB7","#427AB7","#4279B7","#4178B6","#4176B6","#4075B5","#4074B5","#4073B4","#4072B4","#3F71B4","#3F70B3","#3F6FB3","#3F6EB2","#3F6CB2","#3F6BB1","#3E6AB0","#3E69B0","#3E68AF","#3E67AF","#3E66AE","#3E65AD","#3E63AD","#3E62AC","#3E61AB","#3E60AB","#3E5FAA","#3E5EA9","#3E5DA8","#3E5CA7","#3E5AA6","#3F59A5","#3F58A4","#3F57A3","#3F56A2","#3F55A1","#3F54A0","#3F539F","#3F529E","#3F519D","#3F509B","#3F4F9A","#3F4E99","#3F4C97","#3F4B96","#3F4A95","#3E4993","#3E4992","#3E4890","#3E478F","#3E468D","#3E458C","#3D448A","#3D4389","#3D4287","#3C4185","#3C4084","#3C3F82","#3B3E80","#3B3E7F","#3A3D7D","#3A3C7B","#3A3B7A","#393A78","#393976","#383975","#383873","#373771","#363670","#36356E","#35356C","#35346B","#343369","#333267","#333266","#323164","#313062","#312F61","#302F5F","#2F2E5E","#2F2D5C","#2E2C5A","#2D2B59","#2C2B57","#2C2A55","#2B2954","#2A2852","#292851","#29274F","#28264E","#27254C","#26254A","#252449","#252347","#242246","#232244","#222143","#212041","#201F40","#1F1F3E","#1F1E3D","#1E1D3B","#1D1C3A","#1C1C38","#1B1B37","#1A1A35","#191934","#181832","#171831","#17172F","#16162E","#15152C","#14142B","#13132A","#121328","#111227","#101125","#0F1024","#0E0F22","#0D0E21","#0C0D1F","#0B0D1E","#0A0C1D","#090B1B","#080A1A","#070918","#060817","#050715","#050614","#040613"),
    "Tempo",'Sequential', 'CPT-City', c("#FFF6F4","#FDF5F3","#FCF4F1","#FBF3F0","#F9F2EE","#F8F1ED","#F7F0EB","#F5EFEA","#F4EEE8","#F2EDE7","#F1ECE5","#F0EBE4","#EEEAE2","#EDEAE1","#EBE9DF","#EAE8DE","#E9E7DD","#E7E6DB","#E6E5DA","#E4E4D8","#E3E3D7","#E2E2D6","#E0E2D4","#DFE1D3","#DDE0D1","#DCDFD0","#DBDECF","#D9DDCD","#D8DDCC","#D6DCCB","#D5DBC9","#D3DAC8","#D2D9C7","#D1D8C5","#CFD8C4","#CED7C3","#CCD6C1","#CBD5C0","#C9D4BF","#C8D4BE","#C6D3BC","#C5D2BB","#C3D1BA","#C2D1B9","#C0D0B7","#BFCFB6","#BDCEB5","#BCCEB4","#BACDB3","#B9CCB2","#B7CBB0","#B6CBAF","#B4CAAE","#B3C9AD","#B1C8AC","#B0C8AB","#AEC7AA","#ACC6A9","#ABC5A8","#A9C5A6","#A8C4A5","#A6C3A4","#A4C3A3","#A3C2A2","#A1C1A1","#A0C0A0","#9EC09F","#9CBF9F","#9BBE9E","#99BE9D","#97BD9C","#96BC9B","#94BC9A","#92BB99","#91BA98","#8FBA97","#8DB997","#8BB896","#8AB795","#88B794","#86B693","#85B593","#83B592","#81B491","#7FB390","#7DB390","#7CB28F","#7AB18E","#78B18E","#76B08D","#74AF8D","#72AF8C","#71AE8B","#6FAD8B","#6DAD8A","#6BAC8A","#69AB89","#67AB89","#65AA88","#63A988","#61A987","#5FA887","#5DA786","#5BA686","#59A685","#57A585","#56A485","#54A484","#52A384","#50A284","#4EA183","#4BA183","#49A083","#479F82","#459F82","#439E82","#419D82","#3F9C81","#3D9C81","#3B9B81","#3A9A81","#389981","#369880","#349880","#329780","#309680","#2E9580","#2C947F","#2A937F","#29937F","#27927F","#25917F","#24907F","#228F7E","#218E7E","#1F8D7E","#1E8D7E","#1C8C7E","#1B8B7D","#1A8A7D","#19897D","#17887D","#16877C","#16867C","#15857C","#14847C","#13847B","#13837B","#12827B","#12817B","#11807A","#117F7A","#117E7A","#117D79","#117C79","#117B79","#117A78","#117978","#117878","#117777","#117677","#127676","#127576","#127476","#137375","#137275","#137174","#147074","#146F73","#146E73","#156D73","#156C72","#166B72","#166A71","#166971","#176870","#176770","#17666F","#18656F","#18656E","#18646E","#19636D","#19626D","#19616C","#19606C","#1A5F6B","#1A5E6B","#1A5D6A","#1A5C6A","#1A5B69","#1B5A68","#1B5968","#1B5867","#1B5867","#1B5766","#1B5666","#1C5565","#1C5465","#1C5364","#1C5263","#1C5163","#1C5062","#1C4F62","#1C4E61","#1C4D61","#1C4C60","#1C4C5F","#1C4B5F","#1C4A5E","#1C495E","#1C485D","#1C475D","#1C465C","#1C455B","#1C445B","#1C435A","#1C425A","#1C4259","#1C4158","#1C4058","#1B3F57","#1B3E57","#1B3D56","#1B3C56","#1B3B55","#1B3A54","#1B3954","#1B3853","#1A3753","#1A3652","#1A3651","#1A3551","#1A3450","#1A3350","#19324F","#19314F","#19304E","#192F4D","#192E4D","#182D4C","#182C4C","#182B4B","#182A4B","#18294A","#17284A","#172749","#172648","#172548","#172447","#162347","#162246","#162146","#162045","#151F45","#151E44","#151D44"),
    # diverging ramps
    "RwB", 'Diverging', 'ColorBrewer', c("67001F","B2182B","D6604D","F4A582","FDDBC7","F7F7F7","D1E5F0","92C5DE","4393C3","2166AC","053061"),
    "RwBsoft", 'Diverging', 'Custom', c('93021b','e28f76','F4F4F4','8c95ad','2b2d42'),
    "RwBpale", 'Diverging', 'Custom', c("934855","e2b7aa","F4F4F4","9ca0ad","353642"),
    "RyG", 'Diverging', 'ColorBrewer', c("A50026","D73027","F46D43","FDAE61","FEE08B","FFFFBF","D9EF8B","A6D96A","66BD63","1A9850","006837"),
    "PwG", 'Diverging', 'ColorBrewer', c("40004B","762A83","9970AB","C2A5CF","E7D4E8","F7F7F7","D9F0D3","A6DBA0","5AAE61","1B7837","00441B"),
    "OwP", 'Diverging', 'ColorBrewer', c("7F3B08","B35806","E08214","FDB863","FEE0B6","F7F7F7","D8DAEB","B2ABD2","8073AC","542788","2D004B"),
    "Spectral", 'Diverging', 'ColorBrewer', c("9E0142","D53E4F","F46D43","FDAE61","FEE08B","FFFFBF","E6F598","ABDDA4","66C2A5","3288BD","5E4FA2"),
    "Delta", 'Diverging', 'CPT-City', c("#112040","#122143","#142246","#152449","#16254C","#172750","#192853","#1A2956","#1B2A5A","#1C2C5D","#1E2D60","#1F2E64","#203067","#21316B","#22326F","#233372","#243576","#25367A","#25377E","#263882","#263A86","#273B8A","#273D8D","#263E91","#254094","#244297","#234498","#21479A","#20499B","#1F4B9B","#1E4D9C","#1D509C","#1C529D","#1C549D","#1B569D","#1B589E","#1B5A9E","#1B5C9E","#1B5E9F","#1B609F","#1B639F","#1B659F","#1C67A0","#1C69A0","#1D6AA0","#1E6CA1","#1E6EA1","#1F70A2","#2072A2","#2174A2","#2276A3","#2378A3","#247AA4","#257CA4","#267EA5","#2780A5","#2882A6","#2A84A6","#2B85A6","#2C87A7","#2E89A7","#2F8BA8","#308DA8","#328FA9","#3391A9","#3593AA","#3795AA","#3897AB","#3A98AB","#3C9AAC","#3E9CAC","#409EAD","#42A0AD","#45A2AE","#47A3AE","#4AA5AE","#4DA7AF","#50A9AF","#53AAB0","#56ACB0","#5AAEB0","#5DAFB1","#61B1B1","#65B3B2","#69B4B3","#6DB6B3","#71B7B4","#75B8B5","#79BAB5","#7DBBB6","#81BDB7","#84BEB8","#88C0B9","#8CC1BA","#90C3BC","#94C4BD","#97C5BE","#9BC7BF","#9EC8C1","#A2CAC2","#A6CBC3","#A9CDC4","#ACCEC6","#B0D0C7","#B3D2C9","#B7D3CA","#BAD5CC","#BDD6CD","#C1D8CE","#C4DAD0","#C7DBD1","#CADDD3","#CDDFD4","#D1E0D6","#D4E2D7","#D7E4D9","#DAE6DA","#DDE7DB","#E0E9DD","#E3EBDE","#E6EDDF","#EAEFE0","#EDF1E2","#F0F3E3","#F3F5E3","#F6F7E4","#FAF8E5","#FDFAE6","#FFFCCC","#FDFAC8","#FCF7C4","#FAF5BF","#F9F3BB","#F8F0B7","#F6EEB2","#F5EBAE","#F4E9AA","#F2E7A6","#F1E4A1","#F0E29D","#EEE099","#EDDE94","#EBDC90","#EAD98C","#E8D787","#E7D583","#E5D37F","#E4D17A","#E2CF76","#E0CD71","#DFCB6D","#DDC969","#DBC764","#D9C560","#D7C45C","#D5C258","#D2C053","#D0BE4F","#CEBD4B","#CBBB47","#C8BA43","#C6B83F","#C3B73C","#C0B538","#BDB434","#BAB331","#B7B12E","#B4B02A","#B1AF27","#AEAD24","#ABAC21","#A8AB1E","#A4AA1B","#A1A818","#9EA716","#9AA613","#97A511","#93A40E","#90A20C","#8CA10A","#89A009","#859F07","#829E06","#7E9C06","#7B9B06","#779A06","#749906","#709807","#6C9607","#699508","#65940A","#61920B","#5E910D","#5A900E","#578F10","#538D11","#4F8C13","#4C8B15","#488916","#448818","#418619","#3D851B","#3A831C","#36821D","#33801F","#2F7F20","#2C7D21","#297C22","#267A24","#227925","#1F7726","#1D7527","#1A7427","#177228","#157029","#136E2A","#116D2A","#0F6B2B","#0D692B","#0C672C","#0B652C","#0B642C","#0B622D","#0B602D","#0B5E2D","#0C5C2D","#0D5A2D","#0E582D","#0F562C","#0F552C","#10532C","#11512C","#124F2B","#134D2B","#144B2A","#15492A","#164729","#164528","#174327","#174127","#184026","#183E25","#193C24","#193A23","#193822","#193621","#19341F","#19321E","#19301D","#192E1C","#192C1A","#192B19","#182917","#182716","#182514","#172313"),
    "RwBbalance", 'Diverging', 'CPT-City', c("#3C0912","#3F0A13","#410A14","#440B16","#460B17","#490C18","#4B0C19","#4E0D1B","#510D1C","#530D1D","#560E1E","#580E1F","#5B0E20","#5E0E21","#600E22","#630E23","#650F24","#680F25","#6B0F25","#6D0E26","#700E27","#730E27","#760E28","#780E28","#7B0E29","#7E0E29","#800E29","#830E29","#860E29","#880F29","#8B0F29","#8D1029","#901029","#921228","#941328","#971428","#991627","#9B1727","#9D1926","#9F1B26","#A11D25","#A31F25","#A52125","#A72424","#A92624","#AB2924","#AC2B24","#AE2E24","#AF3024","#B13325","#B23525","#B33826","#B43B27","#B63D28","#B74029","#B8432B","#B9452C","#BA482E","#BB4B30","#BC4D32","#BD5034","#BE5236","#BE5538","#BF573A","#C05A3C","#C15C3F","#C25F41","#C36143","#C36346","#C46648","#C5684B","#C66B4D","#C76D50","#C76F53","#C87255","#C97458","#CA765B","#CA795D","#CB7B60","#CC7D63","#CD7F65","#CD8268","#CE846B","#CF866E","#CF8970","#D08B73","#D18D76","#D18F79","#D2927C","#D3947F","#D39681","#D49984","#D59B87","#D69D8A","#D69F8D","#D7A290","#D8A493","#D8A696","#D9A998","#DAAB9B","#DAAD9E","#DBAFA1","#DCB2A4","#DCB4A7","#DDB6AA","#DEB9AD","#DFBBB0","#DFBDB2","#E0C0B5","#E1C2B8","#E1C4BB","#E2C7BE","#E3C9C1","#E4CBC4","#E5CEC7","#E5D0CA","#E6D2CD","#E7D5CF","#E8D7D2","#E9D9D5","#EADCD8","#EBDEDB","#ECE0DE","#EDE3E0","#EEE5E3","#EFE8E6","#F0EAE9","#F1ECEB","#F1ECEC","#EEEAEA","#EBE9E9","#E9E7E7","#E6E5E6","#E3E3E4","#E1E1E3","#DEE0E1","#DBDEE0","#D8DCDE","#D5DADD","#D3D9DC","#D0D7DA","#CDD5D9","#CAD4D8","#C7D2D7","#C4D0D5","#C1CFD4","#BFCDD3","#BCCCD2","#B9CAD0","#B6C9CF","#B3C7CE","#B0C5CD","#ADC4CC","#AAC2CB","#A7C1CA","#A4BFC9","#A1BEC8","#9EBCC7","#9BBBC6","#98BAC5","#94B8C4","#91B7C3","#8EB5C3","#8BB4C2","#88B2C1","#85B1C0","#81AFC0","#7EAEBF","#7BACBF","#78ABBE","#75AABE","#71A8BD","#6EA7BD","#6BA5BC","#68A4BC","#65A2BC","#62A0BB","#5F9FBB","#5C9DBB","#599CBB","#569ABB","#5399BA","#5197BA","#4E95BA","#4B94BA","#4892BA","#4690BA","#438FBA","#408DBA","#3E8BBA","#3B89BA","#3888BA","#3686BA","#3384BA","#3083BA","#2E81BA","#2B7FBA","#297DBA","#267BBA","#237ABA","#2178BB","#1E76BB","#1B74BB","#1972BB","#1670BC","#136EBC","#116CBC","#0F6ABD","#0D68BD","#0B66BD","#0A64BE","#0A62BE","#0A60BE","#0C5EBE","#0D5BBE","#1059BE","#1357BE","#1655BD","#1952BC","#1C50BA","#1F4EB8","#214CB6","#234AB3","#2548B0","#2647AC","#2745A9","#2843A5","#2942A2","#29409E","#293F9A","#293E97","#293C93","#293B8F","#293A8C","#293888","#283784","#283681","#27347D","#27337A","#263276","#253073","#252F6F","#242E6C","#232D69","#222B65","#212A62","#21295F","#20275B","#1F2658","#1E2555","#1D2352","#1C224F","#1B214C","#1A1F49","#191E46","#181C43"),
    
    # palettes
    "MntHydro",'Qualitative','Custom', c('232365','FFCD00','AAAAAA','7b942e','deebf6','202030','FFECA2','454545','dfebbd','9b7f0a','1E5E37'),
    "UUmain",'Qualitative','Utrecht University', c('FFCD00','C00A35','000000','094D8E','36A2C9','791D72','9A0000','F08000','FEF200','419702'),
    "UUalt",'Qualitative','Utrecht University', c('FFE6AB','F3965E','AA1555','6E3B23','24A793','5287C6','001240','5B2182'),
    "Set1",'Qualitative','ColorBrewer',  c("E41A1C","377EB8","4DAF4A","984EA3","FF7F00","FFFF33","A65628","F781BF","999999"),
    "Set2",'Qualitative','ColorBrewer', c("66C2A5","FC8D62","8DA0CB","E78AC3","A6D854","FFD92F","E5C494","B3B3B3"),
    "Set3",'Qualitative','ColorBrewer', c("8DD3C7","FFFFB3","BEBADA","FB8072","80B1D3","FDB462","B3DE69","FCCDE5","D9D9D9","BC80BD","CCEBC5"),
    "Pastel1",'Qualitative','ColorBrewer', c("FBB4AE","B3CDE3","CCEBC5","DECBE4","FED9A6","FFFFCC","E5D8BD","FDDAEC","F2F2F2"),
    "Pastel2",'Qualitative','ColorBrewer', c("B3E2CD","FDCDAC","CBD5E8","F4CAE4","E6F5C9","FFF2AE","F1E2CC","CCCCCC"),
    "Accent",'Qualitative','ColorBrewer',  c("7FC97F","BEAED4","FDC086","FFFF99","386CB0","F0027F","BF5B17","666666"),
    "Dark",'Qualitative','ColorBrewer', c("1B9E77","D95F02","7570B3","E7298A","66A61E","E6AB02","A6761D","666666"),
    "Google",'Qualitative','Internet',  c('008744','0057e7','d62d20','ffa700','000000'),
    "Raven",'Qualitative','Internet', c('0e1a40','222f5b','5d5d5d','946b2d','000000'),
    "Muted",'Qualitative','Internet', c('2e4045','83adb5','c7bbc9','5e3c58','bfb5b2'),
    "Cappuccino",'Qualitative','Internet', c('4b3832','854442','E5DBCF','3c2f2f','be9b7b'),
    "Pale1",'Qualitative','Internet', c('c4b2c2','b38d97','d5aca9','ebcfb2','c5baaf'),
    "Water",'Qualitative','Internet', c('b9d6f2','084c61','061a40','0353a4','2d93ad'),
    "Skin",'Qualitative','Internet', c('d8e2dc','ffe5d9','ffcad4','f4acb7','9d8189'),
    "NS",'Qualitative','Internet', c('06aed5','086788','f0c808','f9e39f','dd1c1a'),
    "Legendary",'Qualitative','Internet', c('5bc0eb','fde74c','9bc53d','e55934','fa7921'),
    "WarmWall",'Qualitative','Internet', c('d1cebe','e07a5f','3d405b','81b29a','f2cc8f'),
    "Oasis",'Qualitative','Internet', c('000000','839788','eee0cb','baa898','bfd7ea'),
    "Earthly",'Qualitative','Internet', c('537d99','def2c8','bbc451','9a9b58','8f451f'),
    "A3",'Qualitative','Internet', c('1c3144','d00000','ffba08','a2aebb','3f88c5'),
    "AfterCold",'Qualitative','Internet', c('171219','225560','edf060','f0803c','310d20'),
    "Swoop",'Qualitative','Internet', c('dd7373','3b3561','ead94c','d1d1d1','51a3a3'),
    "BurbleTeal",'Qualitative','Internet', c('2d3047','93b7be','e0ca3c','a799b7','048a81'),
    "LGBT",'Qualitative','CPT-City', c("#750787","#004DFF","#008026","#FFED00","#FF8C00","#E40303"),
  )
  
  # Error message in case of unknown color name
  if (sum(tolower(coldat$name) %in% tolower(name))==0){
    stop(paste0('Color name not available.\n\nChoose any of:\n',paste(sort(coldat$name),collapse=', '),
                '\n\n','Use ramp(show=TRUE) or pal(show=TRUE) to plot all color ramps/palettes, types and names.'))
  }
  
  # function to extract correct color from table
  getPal <- function(name){
    coldat$colors[[which(tolower(coldat$name) %in% tolower(name))]]
  }
  # function to extract type for color ramp
  getType <- function(name){
    coldat$type[[which(tolower(coldat$name) %in% tolower(name))]]
  }
  
  # force default number
  if (is.null(number)){
    nodefnum <- T
    if(getType(name)=='Qualitative'){
      number <- length(getPal(name))
    }else{
      number <- 100
    }
  }else{
    nodefnum <- F
  }
  
  # force a hash at beginning of the hexcol
  coldat$colors <- lapply(coldat$colors, function(x) tolower(paste0('#',gsub('#','',x))))
  
  # Make plot of available colors
  if (show){ # make plot of available ramps
    x11(bg='#2d2d2d', height=8.65, width=14)
    
    if(nodefnum){
      number <- 50
    }
    
    # split the classes, sort alphabetically, and merge back
    plotdat        <- split(coldat,coldat$type)[c(1,3,2)]
    plotdat        <- do.call(rbind,lapply(plotdat, function(x) x[order(x$name),]))
    
    
    
    # add blank row at type change
    typechange <- which(plotdat$type[-1] != plotdat$type[-nrow(plotdat)])
    plotdat <- rbind(plotdat[1:typechange[1],],
                     c(NA,NA,NA,NA),
                     plotdat[(typechange[1]+1):typechange[2],],
                     c(NA,NA,NA,NA),
                     plotdat[(typechange[2]+1):nrow(plotdat),]
                     )
    rampdat <- plotdat[plotdat$type!='Qualitative',]
    rampdat <- rampdat[-nrow(rampdat),]
    rampdat$colors <- lapply(rampdat$colors, function(x){
      if (length(x)<2){
        return(rep(NA,number))
      }else{
        return(colorRampPalette(x)(number))
      }
    })
    paldat  <- plotdat[plotdat$type=='Qualitative',]
    paldat  <- paldat[-c(1:2),]
    
    # make plot
    op  <- par(mar=c(0.5,9,2.5,6.5), mfcol=c(1,2))
    barheight <- 0.7
    namecex <- 0.9
    typecex <- 1.5
    sourcex <- 0.65
    
    # ramp plot ==========
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,nrow(rampdat)+1), xlim=c(0,number), xaxs='i', yaxs='i')
    for (i in 1:nrow(rampdat)){
      ncolors=number
      for (j in 1:ncolors){
        irev <-  (length(rampdat$name):1)[i]
        polygon(x=c(j-1,j,j,j-1), y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border=NA, col=rampdat$colors[[i]][j])
      }
      if (!is.na(plotdat$name[i])){
        polygon(x=c(0,number,number,0),y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border='black', col=NA, lwd=0.5)
      }
    }
    mtext(rampdat$name, side=2, line=0.5, at=nrow(rampdat):1,padj=0.5,adj=1,las=2, cex=namecex, col='white', font=2)
    mtext(rampdat$source, side=4, line=0.5, at=nrow(rampdat):1,padj=0.5,adj=0,las=2, cex=sourcex, col='white')
    # add class labels
    classpos   <- sapply(split(rev(seq(nrow(rampdat))),rampdat$type), mean)
    mtext(names(classpos), side=2, line=7, cex=typecex, at=classpos, col='white',font=2)
    # add title
    mtext('Color ramps', side=3, line=0.5, cex=typecex*1.2, at=number/2, col='white',font=2)
    
    
    # pal plot =========
    plot(NA,axes=F,xlab='',ylab='',ylim=c(0,nrow(paldat)+1), xlim=c(0,1), xaxs='i', yaxs='i')
    for (i in 1:nrow(paldat)){
      ncolors <- length(paldat$colors[[i]])
      delta   <- 1/ncolors
      for (j in 1:ncolors){
        p <- j/ncolors
        irev <-  (length(paldat$name):1)[i]
        polygon(x=c(p-delta,p,p,p-delta), y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border=NA, col=paldat$colors[[i]][j])
      }
      if (!is.na(plotdat$name[i])){
        polygon(x=c(0,1,1,0),y=c(rep(irev-0.5*barheight,2),rep(irev+0.5*barheight,2)), border='black', col=NA, lwd=0.5)
      }
    }
    mtext(paldat$name, side=2, line=0.5, at=nrow(paldat):1,padj=0.5,adj=1,las=2, cex=namecex, col='white', font=2)
    mtext(paldat$source, side=4, line=0.5, at=nrow(paldat):1,padj=0.5,adj=0,las=2, cex=sourcex, col='gray80')
    # add class labels
    classpos   <- sapply(split(rev(seq(nrow(paldat))),paldat$type), mean)
    mtext(names(classpos), side=2, line=7, cex=typecex, at=classpos, col='white', font=2)
    # add title
    mtext('Color palettes', side=3, line=0.5, cex=typecex*1.2, at=1/2, col='white',font=2)
    
    par(op)
    warning('No color output when "show=TRUE", only an external plot with available ramps and palettes')
    
    
    
  }else{ # generate output
    
    colpal   <- getPal(name)
    coltype  <- getType(name)
    
    if (reversed){colpal <- rev(colpal)}
    if (random){colpal   <- sample(colpal)}
    if (center!=0.5){
      if (coltype == 'Qualitative'){stop('Center should be 0.5 for qualitative palettes.')}
      censcl  <- center*1e3 + 1
      colvec  <- colorRampPalette(colpal)(1e4+1)
      sampind <- approx(y=c(0,500,1000), x=c(0,censcl,1000), xout=0:1000)$y
      sampind <- as.integer(sampind * 10 +1)
      colpal  <- colvec[sampind]
    }
    if (coltype=='Qualitative'){
      if (number>length(colpal)){
        outcolors <- rep(colpal,ceiling(number/length(colpal)))[1:number]
        warning(paste0('The qualitative ',toupper(name),' palette has only ',length(colpal), ' colors. Recycling colors up to desired number.'))
      }else{
        outcolors <- colpal[1:number]
      }
    }else{
      outcolors <- colorRampPalette(colpal)(number)
    }
    return(outcolors)
  }
}

#' @rdname ramp
#' @export
pkRamp <- ramp



#' Load preset color palettes and ramps
#'
#' Load color ramps and palettes from presets. Identical to \code{pkrf::ramp()} besides default pallete.
#' @param name Name of the color ramp (string).
#' @param ... Other arguments passed to \code{pkrf::ramp()}
#' @return Vector with hex colors strings. 
#' @export
pal <- function(name='MntHydro', ...){
  pkrf::ramp(name, ...)
}

#' @rdname pal
#' @export
pkPal <- pal




# convert cpt files to  a color vector
cpt2hexvec <- function(fn, rev=F){
  require(stringr)
  s   <- readLines(fn)
  s   <- s[!grepl('#',s,)]
  s   <- s[!grepl('^B',s)]
  s   <- s[!grepl('^F',s)]
  s   <- s[!grepl('^N',s)]
  s   <- gsub("\\s+", " ",s)
  s   <- str_split(s, ' ')
  if(rev){s <- rev(s)}
  hex <- sapply(s, function(x) rgb(as.numeric(x[2])/255,as.numeric(x[3])/255,as.numeric(x[4])/255))
  cat(paste0('c(',paste0('"',hex,collapse='",'),'"),'))
}
