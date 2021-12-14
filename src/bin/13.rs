use std::collections::HashSet;

static POINTS: &str = "241,142\n57,889\n1131,239\n1076,828\n495,612\n656,775\n977,665\n65,627\n1004,401\n1245,627\n567,750\n1197,194\n654,822\n214,644\n25,738\n256,784\n1044,485\n957,102\n291,673\n271,715\n400,182\n415,245\n1083,777\n995,486\n932,695\n951,590\n376,184\n1200,879\n234,156\n276,807\n1019,221\n1110,171\n764,54\n594,317\n706,711\n348,485\n567,751\n835,360\n239,425\n169,634\n1205,131\n628,159\n363,682\n664,124\n1096,26\n1126,849\n309,217\n341,509\n612,365\n787,878\n1096,560\n378,599\n956,112\n1222,178\n947,682\n514,750\n542,876\n706,560\n514,144\n721,175\n355,511\n736,513\n1260,560\n20,746\n28,211\n721,681\n604,199\n589,715\n140,555\n967,550\n877,565\n1004,605\n157,649\n797,514\n157,612\n376,262\n726,336\n239,701\n1126,798\n1197,700\n1290,596\n390,607\n226,791\n254,182\n1059,565\n195,464\n95,462\n1064,494\n656,248\n1201,159\n1191,843\n157,359\n105,439\n1125,346\n470,350\n840,361\n475,655\n1159,486\n982,241\n515,301\n415,469\n557,175\n179,534\n850,632\n1310,653\n25,828\n469,794\n768,196\n179,140\n776,528\n1265,348\n1049,136\n82,494\n1103,639\n57,5\n706,199\n1131,140\n1010,782\n956,782\n1069,752\n1135,178\n760,109\n30,400\n214,71\n221,539\n915,880\n105,91\n751,432\n1131,886\n803,803\n194,794\n668,386\n475,136\n738,809\n485,604\n900,520\n1305,306\n1258,186\n490,184\n276,711\n1140,198\n1155,556\n393,345\n349,156\n74,682\n716,409\n835,463\n537,585\n343,550\n642,386\n264,607\n445,809\n353,102\n147,136\n654,72\n1159,408\n716,317\n1153,535\n1153,649\n1246,523\n1201,761\n77,599\n214,876\n85,711\n882,551\n594,577\n17,117\n808,464\n102,485\n181,117\n930,193\n485,148\n522,411\n88,548\n82,641\n415,873\n430,261\n850,140\n841,346\n661,761\n1066,465\n80,381\n612,529\n604,830\n922,633\n557,271\n982,38\n105,117\n773,757\n1064,752\n1228,641\n176,50\n682,159\n947,660\n278,126\n1071,425\n1282,379\n1083,565\n75,730\n830,883\n1215,350\n989,540\n50,707\n30,259\n888,11\n1201,180\n632,773\n897,752\n1096,868\n1039,715\n841,548\n604,560\n1228,361\n604,407\n542,606\n781,614\n716,45\n321,164\n1305,588\n378,466\n1262,38\n523,430\n1015,142\n1250,577\n321,87\n455,451\n301,794\n1201,831\n87,751\n1078,459\n227,777\n818,870\n708,260\n1207,44\n967,136\n1071,693\n1230,694\n554,287\n937,285\n1275,862\n1096,879\n868,40\n728,264\n1255,120\n1258,193\n898,31\n485,232\n493,432\n8,882\n244,353\n50,560\n649,596\n914,211\n1009,550\n162,248\n765,266\n1029,197\n999,662\n1116,262\n922,200\n1258,813\n549,551\n132,754\n1004,289\n1228,494\n1148,248\n266,409\n507,310\n87,591\n1148,465\n1282,211\n676,298\n1213,773\n1076,724\n192,95\n586,871\n813,130\n422,883\n1203,395\n994,435\n835,136\n835,239\n408,513\n321,535\n1026,814\n278,38\n721,715\n684,543\n1210,103\n1062,129\n795,301\n80,246\n547,296\n266,697\n109,133\n1260,793\n433,565\n1288,121\n31,128\n422,11\n343,214\n537,137\n113,418\n663,786\n584,269\n428,551\n821,880\n475,491\n773,533\n1236,660\n396,211\n589,213\n848,543\n900,429\n818,24\n109,581\n105,791\n880,261\n1096,242\n602,81\n151,520\n363,221\n7,338\n840,466\n507,214\n316,710\n1101,796\n639,662\n565,502\n934,574\n803,584\n922,493\n261,758\n1091,395\n766,879\n1245,267\n1124,681\n654,418\n1245,696\n266,493\n1207,626\n646,124\n1076,211\n1277,712\n1054,784\n5,456\n594,409\n103,268\n929,210\n1211,329\n731,255\n181,777\n647,556\n572,494\n962,401\n788,698\n1052,423\n363,212\n633,380\n801,565\n856,378\n1208,25\n1245,715\n348,849\n760,561\n731,117\n502,235\n266,485\n691,227\n523,878\n1310,241\n877,833\n363,225\n221,61\n903,73\n656,822\n1019,212\n1011,623\n55,774\n1283,271\n962,267\n687,80\n408,142\n1223,751\n691,480\n716,401\n1289,828\n1206,675\n1131,360\n967,214\n566,794\n214,375\n162,86\n1148,86\n301,290\n681,290\n396,683\n268,101\n455,443\n1284,560\n1236,234\n199,443\n490,486\n869,700\n684,473\n768,197\n1021,726\n311,680\n803,399\n856,516\n467,334\n1066,30\n92,514\n184,849\n572,137\n574,513\n1235,117\n870,252\n769,194\n661,92\n97,719\n525,294\n328,241\n95,350\n470,361\n1230,246\n972,177\n1205,803\n373,285\n142,25\n475,534\n502,883\n1011,63\n594,401\n594,849\n1253,889\n1096,625\n162,808\n708,708\n807,374\n1071,21\n621,854\n432,449\n761,509\n1268,529\n619,862\n185,346\n989,140\n333,730\n1287,789\n167,730\n430,633\n378,295\n315,486\n1143,612\n266,633\n619,480\n237,54\n1034,183\n768,249\n321,428\n284,814\n53,12\n492,248\n773,85\n803,455\n989,428\n1260,606\n266,381\n447,502\n890,66\n807,626\n826,646\n721,719\n1298,386\n1260,334\n639,232\n984,241\n761,131\n75,164\n1036,641\n1216,883\n333,665\n888,256\n388,872\n549,385\n1146,520\n522,262\n408,400\n843,334\n194,100\n151,486\n151,626\n65,719\n972,688\n407,73\n475,808\n353,100\n788,632\n917,345\n740,98\n1071,193\n1153,282\n420,549\n1044,513\n129,600\n604,549\n545,628\n258,311\n251,565\n590,523\n584,558\n1116,885\n75,117\n924,793\n985,350\n336,98\n412,31\n803,758\n929,628\n972,733\n604,194\n415,425\n1277,40\n1101,85\n545,266\n1215,462\n798,86\n661,298\n1049,399\n1235,526\n855,451\n604,269\n840,533\n1129,117\n412,191\n945,278\n246,374\n1293,117\n698,365\n378,428\n623,80\n276,421\n411,381\n1248,567\n489,14\n50,101\n351,105\n661,596\n634,596\n1078,11\n187,782\n52,365\n1103,502\n21,290\n261,399\n70,522\n768,700\n547,395\n1168,18\n1170,144\n1131,207\n1034,635\n224,633\n1198,697\n1123,334\n74,234\n1034,259\n132,632\n460,754\n179,8\n142,186\n157,805\n169,164\n26,560\n363,669\n1257,882\n234,828\n1084,607\n219,403\n788,483\n475,86\n738,757\n551,268\n999,214\n720,371\n731,777\n184,798\n1305,802\n1056,264\n1062,765\n1287,341\n300,782\n589,457\n194,306\n351,119\n879,44\n1158,435\n333,754\n686,35\n807,38\n1201,628\n604,569\n179,136\n513,514\n264,119\n207,502\n918,75\n386,793\n103,626\n281,53\n416,614\n557,623\n678,121\n898,191\n562,784\n291,3\n23,229\n112,698\n83,513\n1091,499\n604,625\n929,511\n917,121\n306,858\n567,303\n60,409\n284,80\n1038,168\n1290,298\n417,840\n475,239\n365,278\n10,357\n460,710\n1273,278\n914,722\n276,635\n107,395\n326,38\n232,11\n274,813\n52,36\n761,551\n105,803\n97,47\n837,777\n1134,760\n142,96\n738,400\n922,175\n1032,768\n646,434\n877,329\n179,758\n378,487\n1034,421\n162,429\n60,241\n460,306\n1246,371\n689,854\n349,548\n289,616\n175,178\n1233,599\n1257,133\n574,648\n1159,520\n1086,633\n914,683\n691,526\n706,700\n239,245\n1287,229\n175,716\n276,183\n820,856\n837,117\n1103,703\n1170,555\n1260,288\n1034,199\n1260,707\n410,80\n1141,306\n567,591\n348,401\n169,306\n276,259\n251,789\n1302,460\n706,382\n807,520\n989,164\n1021,840\n989,730\n435,296\n1115,464\n1110,826\n816,415\n736,246\n507,584\n776,366\n299,271\n1289,290\n894,614\n813,425\n325,350\n321,354\n1001,217\n957,100\n1285,828\n764,840\n392,819\n529,614\n1200,431\n1049,267\n1280,259\n316,206\n580,660\n207,639\n5,306\n137,603\n706,247\n184,45\n550,333\n50,869\n289,168\n1043,492\n10,296\n1303,108\n184,533\n303,105\n679,889\n934,618\n649,438\n462,351\n23,553\n894,728\n1201,714\n1004,858\n880,421\n1250,689\n666,849\n272,726\n214,652\n316,733\n485,341\n1258,529\n42,813\n484,198\n763,403\n306,401\n381,658\n863,278\n763,499\n28,351\n388,633\n1287,889\n35,227\n929,658\n232,435\n773,585\n721,457\n343,758\n671,232\n922,694\n1088,667\n661,754\n803,491\n1032,38\n567,865\n494,415\n507,491\n1148,696\n1116,588\n1223,469\n869,28\n5,140\n667,38\n195,528";
static FOLDS: &str = "fold along x=655\nfold along y=447\nfold along x=327\nfold along y=223\nfold along x=163\nfold along y=111\nfold along x=81\nfold along y=55\nfold along x=40\nfold along y=27\nfold along y=13\nfold along y=6";

fn fold_grid(points: &HashSet<(i32,i32)>, (dir,pos): (char,i32)) -> HashSet<(i32,i32)> {
  points.iter()
    .map(|&(x,y)| match (dir,x,y) {
      ('x',x,y) if x < pos => (x,y),
      ('x',x,y) => (pos*2 - x,y),
      ('y',x,y) if y < pos => (x,y),
      ('y',x,y) => (x,pos*2 - y),
      _ => unreachable!()
    })
    .collect()
}

fn part2(grid: HashSet<(i32,i32)>, folds: &[(char,i32)]) {
  let final_grid = folds.iter().fold(grid, |grid,&fold| fold_grid(&grid, fold));
  let max_x = final_grid.iter().map(|&(x,_)| x).max().unwrap();
  let max_y = final_grid.iter().map(|&(_,y)| y).max().unwrap();
  for y in 0..=max_y {
    for x in 0..=max_x {
      let c = if final_grid.contains(&(x,y)) {'█'} else {' '};
      print!("{}", c);
    }
    println!();
  }
}

aoc2021::main! {
  let grid = POINTS.lines()
    .map(|l| {
      let (a,b) = l.split_once(',').unwrap();
      (a.parse().unwrap(), b.parse().unwrap())
    })
    .collect::<HashSet<_>>();
  let folds = FOLDS.lines()
    .map(|l| {
      let tmp = l.split_whitespace().last().unwrap();
      let (dir,pos) = tmp.split_once('=').unwrap();
      (dir.as_bytes()[0] as char, pos.parse().unwrap())
    })
    .collect::<Vec<_>>();
  let p1 = fold_grid(&grid, folds[0]).len();
  part2(grid, &folds);
  (p1,"PFKLKCFP")
}