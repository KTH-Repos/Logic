
% For sicstus: use_module(library(lists)).  before consulting the file.

run_all_tests(ProgramToTest) :-
    catch(consult(ProgramToTest),
          B,
          (write('Could not consult \"'), write(ProgramToTest),
           write('\": '), write(B), nl, halt)),
    all_valid_ok(['validown0.txt','valid000.txt','valid001.txt','valid002.txt','valid004.txt','valid005.txt',
'valid006.txt','valid011.txt','valid012.txt','valid014.txt','valid016.txt',
'valid018.txt','valid022.txt','valid023.txt','valid024.txt','valid031.txt',
'valid032.txt','valid035.txt','valid036.txt','valid037.txt','valid038.txt',
'valid042.txt','valid043.txt','valid053.txt','valid054.txt','valid056.txt',
'valid060.txt','valid061.txt','valid063.txt','valid065.txt','valid068.txt',
'valid070.txt','valid073.txt','valid079.txt','valid081.txt','valid085.txt',
'valid086.txt','valid087.txt','valid093.txt','valid098.txt','valid099.txt',
'valid102.txt','valid107.txt','valid112.txt','valid117.txt','valid121.txt',
'valid124.txt','valid133.txt','valid136.txt','valid137.txt','valid145.txt',
'valid146.txt','valid147.txt','valid151.txt','valid153.txt','valid157.txt',
'valid161.txt','valid163.txt','valid173.txt','valid175.txt','valid176.txt',
'valid181.txt','valid182.txt','valid186.txt','valid187.txt','valid188.txt',
'valid190.txt','valid192.txt','valid193.txt','valid199.txt','valid201.txt',
'valid203.txt','valid215.txt','valid218.txt','valid219.txt','valid220.txt',
'valid224.txt','valid225.txt','valid226.txt','valid229.txt','valid232.txt',
'valid233.txt','valid242.txt','valid245.txt','valid249.txt','valid255.txt',
'valid259.txt','valid263.txt','valid265.txt','valid269.txt','valid271.txt',
'valid274.txt','valid275.txt','valid276.txt','valid279.txt','valid280.txt',
'valid281.txt','valid289.txt','valid290.txt','valid301.txt','valid304.txt',
'valid305.txt','valid314.txt','valid322.txt','valid327.txt','valid346.txt',
'valid348.txt','valid351.txt','valid355.txt','valid356.txt','valid360.txt',
'valid364.txt','valid366.txt','valid380.txt','valid381.txt','valid389.txt',
'valid391.txt','valid393.txt','valid394.txt','valid397.txt','valid398.txt',
'valid400.txt','valid403.txt','valid407.txt','valid409.txt','valid410.txt',
'valid411.txt','valid417.txt','valid420.txt','valid421.txt','valid426.txt',
'valid438.txt','valid441.txt','valid443.txt','valid447.txt','valid451.txt',
'valid453.txt','valid455.txt','valid459.txt','valid460.txt','valid465.txt',
'valid468.txt','valid474.txt','valid479.txt','valid493.txt','valid506.txt',
'valid515.txt','valid516.txt','valid518.txt','valid519.txt','valid521.txt',
'valid523.txt','valid530.txt','valid541.txt','valid550.txt','valid552.txt',
'valid558.txt','valid559.txt','valid578.txt','valid585.txt','valid586.txt',
'valid588.txt','valid592.txt','valid593.txt','valid594.txt','valid597.txt',
'valid598.txt','valid599.txt','valid609.txt','valid621.txt','valid627.txt',
'valid628.txt','valid631.txt','valid635.txt','valid640.txt','valid648.txt',
'valid650.txt','valid652.txt','valid653.txt','valid661.txt','valid664.txt',
'valid666.txt','valid667.txt','valid668.txt','valid670.txt','valid673.txt',
'valid675.txt','valid686.txt','valid687.txt','valid689.txt','valid699.txt',
'valid703.txt','valid706.txt','valid710.txt','valid715.txt','valid721.txt',
'valid728.txt','valid738.txt','valid747.txt','valid753.txt','valid756.txt',
'valid768.txt','valid785.txt','valid792.txt','valid797.txt','valid800.txt',
'valid806.txt','valid811.txt','valid814.txt','valid823.txt','valid826.txt',
'valid832.txt','valid833.txt','valid835.txt','valid836.txt','valid839.txt',
'valid840.txt','valid842.txt','valid843.txt','valid849.txt','valid852.txt',
'valid855.txt','valid859.txt','valid862.txt','valid865.txt','valid872.txt',
'valid873.txt','valid874.txt','valid882.txt','valid883.txt','valid887.txt',
'valid888.txt','valid896.txt','valid903.txt','valid905.txt','valid910.txt',
'valid911.txt','valid912.txt','valid914.txt','valid917.txt','valid934.txt',
'valid939.txt','valid951.txt','valid953.txt','valid968.txt','valid986.txt',
'valid987.txt','valid999.txt']),
    all_invalid_ok(['invalidown0.txt','invalid007.txt','invalid009.txt','invalid013.txt','invalid015.txt','invalid017.txt',
'invalid019.txt','invalid020.txt','invalid025.txt','invalid026.txt','invalid027.txt',
'invalid028.txt','invalid029.txt','invalid030.txt','invalid039.txt','invalid041.txt',
'invalid044.txt','invalid045.txt','invalid048.txt','invalid050.txt','invalid051.txt',
'invalid055.txt','invalid064.txt','invalid067.txt','invalid071.txt','invalid074.txt',
'invalid078.txt','invalid082.txt','invalid089.txt','invalid090.txt','invalid091.txt',
'invalid096.txt','invalid097.txt','invalid101.txt','invalid104.txt','invalid105.txt',
'invalid106.txt','invalid110.txt','invalid111.txt','invalid113.txt','invalid114.txt',
'invalid115.txt','invalid118.txt','invalid120.txt','invalid122.txt','invalid123.txt',
'invalid125.txt','invalid126.txt','invalid128.txt','invalid129.txt','invalid131.txt',
'invalid132.txt','invalid134.txt','invalid139.txt','invalid140.txt','invalid141.txt',
'invalid142.txt','invalid143.txt','invalid144.txt','invalid148.txt','invalid149.txt',
'invalid150.txt','invalid152.txt','invalid154.txt','invalid156.txt','invalid158.txt',
'invalid160.txt','invalid165.txt','invalid166.txt','invalid168.txt','invalid169.txt',
'invalid171.txt','invalid172.txt','invalid174.txt','invalid178.txt','invalid180.txt',
'invalid183.txt','invalid184.txt','invalid189.txt','invalid191.txt','invalid194.txt',
'invalid195.txt','invalid196.txt','invalid197.txt','invalid202.txt','invalid205.txt',
'invalid206.txt','invalid207.txt','invalid208.txt','invalid209.txt','invalid210.txt',
'invalid211.txt','invalid213.txt','invalid214.txt','invalid216.txt','invalid217.txt',
'invalid221.txt','invalid223.txt','invalid228.txt','invalid230.txt','invalid231.txt',
'invalid234.txt','invalid237.txt','invalid238.txt','invalid239.txt','invalid241.txt',
'invalid244.txt','invalid246.txt','invalid247.txt','invalid248.txt','invalid250.txt',
'invalid251.txt','invalid252.txt','invalid253.txt','invalid254.txt','invalid256.txt',
'invalid257.txt','invalid258.txt','invalid260.txt','invalid262.txt','invalid264.txt',
'invalid266.txt','invalid268.txt','invalid270.txt','invalid273.txt','invalid277.txt',
'invalid283.txt','invalid285.txt','invalid286.txt','invalid288.txt','invalid291.txt',
'invalid292.txt','invalid293.txt','invalid294.txt','invalid295.txt','invalid296.txt',
'invalid297.txt','invalid302.txt','invalid303.txt','invalid307.txt','invalid309.txt',
'invalid310.txt','invalid313.txt','invalid315.txt','invalid316.txt','invalid317.txt',
'invalid318.txt','invalid319.txt','invalid320.txt','invalid321.txt','invalid323.txt',
'invalid325.txt','invalid326.txt','invalid328.txt','invalid329.txt','invalid330.txt',
'invalid331.txt','invalid332.txt','invalid333.txt','invalid334.txt','invalid335.txt',
'invalid336.txt','invalid339.txt','invalid340.txt','invalid342.txt','invalid343.txt',
'invalid344.txt','invalid345.txt','invalid347.txt','invalid349.txt','invalid352.txt',
'invalid353.txt','invalid354.txt','invalid357.txt','invalid358.txt','invalid359.txt',
'invalid361.txt','invalid365.txt','invalid368.txt','invalid371.txt','invalid373.txt',
'invalid376.txt','invalid377.txt','invalid378.txt','invalid379.txt','invalid383.txt',
'invalid384.txt','invalid385.txt','invalid386.txt','invalid387.txt','invalid388.txt',
'invalid392.txt','invalid395.txt','invalid399.txt','invalid401.txt','invalid404.txt',
'invalid412.txt','invalid414.txt','invalid415.txt','invalid416.txt','invalid419.txt',
'invalid423.txt','invalid424.txt','invalid425.txt','invalid428.txt','invalid429.txt',
'invalid431.txt','invalid432.txt','invalid435.txt','invalid436.txt','invalid439.txt',
'invalid440.txt','invalid442.txt','invalid444.txt','invalid445.txt','invalid446.txt',
'invalid449.txt','invalid454.txt','invalid456.txt','invalid457.txt','invalid458.txt',
'invalid461.txt','invalid462.txt','invalid464.txt','invalid466.txt','invalid467.txt',
'invalid469.txt','invalid470.txt','invalid471.txt','invalid473.txt','invalid475.txt',
'invalid476.txt','invalid481.txt','invalid482.txt','invalid483.txt','invalid485.txt',
'invalid487.txt','invalid489.txt','invalid491.txt','invalid500.txt','invalid504.txt',
'invalid507.txt','invalid508.txt','invalid510.txt','invalid511.txt','invalid517.txt',
'invalid526.txt','invalid528.txt','invalid529.txt','invalid531.txt','invalid536.txt',
'invalid537.txt','invalid539.txt','invalid540.txt','invalid542.txt','invalid544.txt',
'invalid545.txt','invalid546.txt','invalid548.txt','invalid554.txt','invalid556.txt',
'invalid557.txt','invalid561.txt','invalid562.txt','invalid563.txt','invalid564.txt',
'invalid566.txt','invalid569.txt','invalid570.txt','invalid571.txt','invalid572.txt',
'invalid573.txt','invalid580.txt','invalid582.txt','invalid583.txt','invalid587.txt',
'invalid589.txt','invalid591.txt','invalid601.txt','invalid602.txt','invalid603.txt',
'invalid604.txt','invalid606.txt','invalid607.txt','invalid608.txt','invalid610.txt',
'invalid613.txt','invalid617.txt','invalid618.txt','invalid620.txt','invalid624.txt',
'invalid626.txt','invalid629.txt','invalid630.txt','invalid632.txt','invalid633.txt',
'invalid634.txt','invalid636.txt','invalid638.txt','invalid639.txt','invalid641.txt',
'invalid642.txt','invalid645.txt','invalid646.txt','invalid647.txt','invalid651.txt',
'invalid654.txt','invalid655.txt','invalid657.txt','invalid658.txt','invalid659.txt',
'invalid662.txt','invalid663.txt','invalid665.txt','invalid669.txt','invalid671.txt',
'invalid672.txt','invalid674.txt','invalid676.txt','invalid678.txt','invalid680.txt',
'invalid681.txt','invalid683.txt','invalid684.txt','invalid685.txt','invalid688.txt',
'invalid690.txt','invalid691.txt','invalid692.txt','invalid693.txt','invalid694.txt',
'invalid697.txt','invalid698.txt','invalid700.txt','invalid701.txt','invalid707.txt',
'invalid708.txt','invalid709.txt','invalid712.txt','invalid713.txt','invalid714.txt',
'invalid718.txt','invalid720.txt','invalid723.txt','invalid725.txt','invalid727.txt',
'invalid729.txt','invalid731.txt','invalid732.txt','invalid733.txt','invalid735.txt',
'invalid737.txt','invalid740.txt','invalid741.txt','invalid743.txt','invalid745.txt',
'invalid746.txt','invalid748.txt','invalid749.txt','invalid751.txt','invalid752.txt',
'invalid754.txt','invalid755.txt','invalid757.txt','invalid758.txt','invalid759.txt',
'invalid760.txt','invalid761.txt','invalid770.txt','invalid771.txt','invalid772.txt',
'invalid773.txt','invalid774.txt','invalid775.txt','invalid778.txt','invalid780.txt',
'invalid782.txt','invalid789.txt','invalid791.txt','invalid793.txt','invalid795.txt',
'invalid796.txt','invalid799.txt','invalid801.txt','invalid802.txt','invalid803.txt',
'invalid805.txt','invalid807.txt','invalid809.txt','invalid812.txt','invalid815.txt',
'invalid816.txt','invalid818.txt','invalid819.txt','invalid822.txt','invalid824.txt',
'invalid825.txt','invalid827.txt','invalid828.txt','invalid829.txt','invalid830.txt',
'invalid831.txt','invalid834.txt','invalid837.txt','invalid838.txt','invalid841.txt',
'invalid844.txt','invalid848.txt','invalid853.txt','invalid854.txt','invalid856.txt',
'invalid857.txt','invalid858.txt','invalid860.txt','invalid861.txt','invalid863.txt',
'invalid866.txt','invalid870.txt','invalid875.txt','invalid876.txt','invalid877.txt',
'invalid878.txt','invalid879.txt','invalid881.txt','invalid884.txt','invalid885.txt',
'invalid889.txt','invalid890.txt','invalid891.txt','invalid894.txt','invalid895.txt',
'invalid898.txt','invalid901.txt','invalid902.txt','invalid906.txt','invalid907.txt',
'invalid909.txt','invalid913.txt','invalid915.txt','invalid920.txt','invalid921.txt',
'invalid922.txt','invalid923.txt','invalid925.txt','invalid926.txt','invalid929.txt',
'invalid931.txt','invalid933.txt','invalid935.txt','invalid936.txt','invalid938.txt',
'invalid940.txt','invalid941.txt','invalid943.txt','invalid944.txt','invalid945.txt',
'invalid946.txt','invalid947.txt','invalid948.txt','invalid950.txt','invalid952.txt',
'invalid956.txt','invalid957.txt','invalid960.txt','invalid961.txt','invalid963.txt',
'invalid965.txt','invalid966.txt','invalid970.txt','invalid974.txt','invalid976.txt',
'invalid977.txt','invalid979.txt','invalid980.txt','invalid983.txt','invalid984.txt',
'invalid989.txt','invalid990.txt','invalid991.txt','invalid993.txt','invalid995.txt',
'invalid996.txt','invalid997.txt','invalid998.txt']),
    halt.
    
all_valid_ok([]).
all_valid_ok([Test | Remaining]) :-
    write(Test), 
    (verify(Test), write(' passed');
    write(' failed. The proof is valid but your program rejected it!')),
    nl, all_valid_ok(Remaining).

all_invalid_ok([]).
all_invalid_ok([Test | Remaining]) :-
    write(Test), 
    (\+verify(Test), write(' passed');
    write(' failed. The proof is invalid but your program accepted it!')),
    nl, all_invalid_ok(Remaining).
