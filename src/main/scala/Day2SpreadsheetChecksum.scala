package jpickard.adventofcode

class Spreadsheet(val data: List[List[Int]]) {

//  def this(h: Int, w: Int) = this(List.fill(h)(List.fill(w)("")))

  def this(strdata: String) = {
    this(
      strdata.split("\n").map(
        line => (line.replaceAll(" +"," ").replaceAll("\t"," ").split(" ").map(_.toInt)).toList
      ).toList
    )
  }

  def checksumDiff: Int = {
    data.map( row =>
      row.max - row.min
    ).foldLeft(0)((b,a) => b + a)
  }

  def checksumDiv: Int = _checksumDiv(data)

  def _checksumDiv(d: List[List[Int]]): Int = {
    d.isEmpty match {
      case true => 0
      case false => _checksumDivRow(d.head) + _checksumDiv(d.tail)
    }
  }

  def _checksumDivRow(d: List[Int]): Int = {
    d.tail.isEmpty match {
      case true => 0
      case false => _checksumDivItem(d.head, d.tail) + _checksumDivRow(d.tail)
    }
  }

  // check if an Int is a divisor of any numbers in a List
  def _checksumDivItem(i: Int, d: List[Int]): Int = {
    val (num, div) = i > d.head match {
      case false => (d.head, i)
      case true => (i, d.head)
    }

    num % div match {
      case 0 => num / div
      case _ => if (d.tail.isEmpty) 0 else _checksumDivItem(i, d.tail)
    }
  }
}

object Day2SpreadsheetChecksum {
  def main(args: Array[String]): Unit = {
    val ss = new Spreadsheet("5 1 9 5\n7 5   3\n2 4 6 8")
    println(ss.data, ss.checksumDiff)

    val ss3 = new Spreadsheet("5 9 2 8\n9 4 7 3\n3 8 6 5")
    println(ss3.data, ss3.checksumDiv)

    val ss2 = new Spreadsheet("1224\t926\t1380\t688\t845\t109\t118\t88\t1275\t1306\t91\t796\t102\t1361\t27\t995\n1928\t2097\t138\t1824\t198\t117\t1532\t2000\t1478\t539\t1982\t125\t1856\t139\t475\t1338\n848\t202\t1116\t791\t1114\t236\t183\t186\t150\t1016\t1258\t84\t952\t1202\t988\t866\n946\t155\t210\t980\t896\t875\t925\t613\t209\t746\t147\t170\t577\t942\t475\t850\n1500\t322\t43\t95\t74\t210\t1817\t1631\t1762\t128\t181\t716\t171\t1740\t145\t1123\n3074\t827\t117\t2509\t161\t206\t2739\t253\t2884\t248\t3307\t2760\t2239\t1676\t1137\t3055\n183\t85\t143\t197\t243\t72\t291\t279\t99\t189\t30\t101\t211\t209\t77\t198\n175\t149\t259\t372\t140\t250\t168\t142\t146\t284\t273\t74\t162\t112\t78\t29\n169\t578\t97\t589\t473\t317\t123\t102\t445\t217\t144\t398\t510\t464\t247\t109\n3291\t216\t185\t1214\t167\t495\t1859\t194\t1030\t3456\t2021\t1622\t3511\t222\t3534\t1580\n2066\t2418\t2324\t93\t1073\t82\t102\t538\t1552\t962\t91\t836\t1628\t2154\t2144\t1378\n149\t963\t1242\t849\t726\t1158\t164\t1134\t658\t161\t1148\t336\t826\t1303\t811\t178\n3421\t1404\t2360\t2643\t3186\t3352\t1112\t171\t168\t177\t146\t1945\t319\t185\t2927\t2289\n543\t462\t111\t459\t107\t353\t2006\t116\t2528\t56\t2436\t1539\t1770\t125\t2697\t2432\n1356\t208\t5013\t4231\t193\t169\t3152\t2543\t4430\t4070\t4031\t145\t4433\t4187\t4394\t1754\n5278\t113\t4427\t569\t5167\t175\t192\t3903\t155\t1051\t4121\t5140\t2328\t203\t5653\t3233")
    println(ss2.checksumDiff)
    println(ss2.checksumDiv)
  }
}
