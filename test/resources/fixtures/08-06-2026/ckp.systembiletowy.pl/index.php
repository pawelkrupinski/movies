<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html" />
<meta http-equiv="Cache-Control" content="no-store, no-cache, must-revalidate, post-check=0, pre-check=0" />
    <meta name="title" content="Demo - systembiletowy.pl" />
<meta name="description" content="demo, sprzedaz, biletow" />
    <title>MCK Płońsk - Sprzedaż Biletów Online</title>
    <link rel="icon" type="image/png" href="/favicon_vt.png"><!-- Major Browsers -->
	<!--[if IE]><link rel="SHORTCUT ICON" href="/favicon_vt.ico"/><![endif]--><!-- Internet Explorer-->    
	<!--<link rel="shortcut icon" href="/favicon_vt.ico" />-->
    <link rel="stylesheet" type="text/css" media="screen" href="/css/ckp/system_portal.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/custom-theme/jquery-ui-1.8.18.custom.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/jquery.ui.selectmenu.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/validation.engine.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/kbf_cms/jquery.selectbox.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/uploadify.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/jquery.imagecrop.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/joyride/joyride-2.1.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/select2.min.css" />
    <script type="text/javascript" src="/js/sb/jquery/jquery-1.7.1.js"></script>
<script type="text/javascript" src="/js/sb/jquery/Class.js"></script>
<script type="text/javascript" src="/js/sb/jquery/jquery-ui-1.8.13.custom.min.js"></script>
<script type="text/javascript" src="/js/sb/jquery/jquery-ui-i18n.js"></script>
<script type="text/javascript" src="/sbCartPlugin/js/cart.frontend.js"></script>
<script type="text/javascript" src="/js/sb/jquery/jquery.ui.selectmenu-old.js"></script>
<script type="text/javascript" src="/js/sb/jquery.ui.core.js"></script>
<script type="text/javascript" src="/js/sb/jquery.ui.widget.js"></script>
<script type="text/javascript" src="/js/sb/jquery.ui.position.js"></script>
<script type="text/javascript" src="/js/jquery.validationengine.js"></script>
<script type="text/javascript" src="/js/jquery.mask.js"></script>
<script type="text/javascript" src="/js/jquery.bgscroll.js"></script>
<script type="text/javascript" src="/js/cufon-yui.js"></script>
<script type="text/javascript" src="/js/aller.font.js"></script>
<script type="text/javascript" src="/js/kbf_cms/jquery.selectbox.js"></script>
<script type="text/javascript" src="/js/visualnet.utils.js"></script>
<script type="text/javascript" src="/js/visualnet.modal.js"></script>
<script type="text/javascript" src="/js/uploadify/jquery.uploadify.js"></script>
<script type="text/javascript" src="/js/sb/jquery/jquery.imagecrop.min.js"></script>
<script type="text/javascript" src="/js/joyride/jquery.joyride-2.1.js"></script>
<script type="text/javascript" src="/js/sb/jquery.form.min.js"></script>
<script type="text/javascript" src="/js/select2.min.js"></script>
  </head>
  <body id="body">
    <div class="body">
      <div id="container">
        		
        <div id="navigation">
						<a title="Strona główna" href="http://mckplonsk.pl"><img style="" src="http://www.mckplonsk.pl/wp-content/uploads/2015/10/1941246.png" /></a>
        </div>
          
        <div style="margin: 20px auto; text-align: center;color: red;display: none;" id="visualnet-monit-browser" class="browser-monit">
            Wymagana przeglądarka Chrome, Firefox lub Opera w najnowszej wersji. Prosimy zaktualizuj swoją przeglądarkę i zapraszamy ponownie.
        </div>
                    
        <script type="text/javascript">
  var cart = null;
  $(function () {
    cart = new Cart('https://ckp.systembiletowy.pl/service.php');
    cart.session = 'hkdkq8ijf8vnckjlqnmbro3dkn';
  });
</script>
	
          <div style="height: 77px;">
            <div id="moving_cart" class="cart_top">
              <div id="cart_section" style="position: relative;top: 0px;margin: auto;">				
                <a id="hide_cart" href="#">x</a>			
                <table>
                  <tr>
                    <th class="icon">
						<img style="width: 24px;height: 20px" src="/images/cart_icon.png" /><br/>koszyk                    </th>
                    <td class="content">
                      <div id="sb_cart">
                        <div class="cart_info">
                          <div>Pozycji w koszyku: <span class="count">0</span></div>
                          <div>Wartość koszyka: <span class="worth">0,00 zł</span></div>
                          <div style="clear:both"></div>
                        </div>
                      </div>
                    </td>
                    <td class="see_cart">
                      <div class="cart_actions" id="sb_cart_header">                    
                        <div class="button" id="sb_cart_detail_label">pokaż szczegóły koszyka</div>									
                        <div style="clear:both"></div>
                      </div>
                    </td>
                    <td class="action">
                      <div class="button" id="cart_buy_button"><a class="cart_buy_button" href="#">przejdź do zakupu</a></div>
                      <span class="linkValue"></span>
                    </td>
                  </tr>
                </table>									
              </div>
            </div>
          </div>
        <div id="content">		 
          <!--<a href="javascript:history.back();" title="wstecz" class="button">« </a>		-->
                        
          <div><table id="rep2" class="table_info tbl_repertoire">
<tr>
	<td style="font-size: 20px;font-weight: bold;color: #5f5f5f;" colspan="4">Seanse Kinowe</td>
</tr>
<tr>
	<th class="tl">Tytuł</th>
	<th>Lokalizacja</th>
	<th>Data</th>
	<th class="tr">Kup bilet</th>
</tr>

		
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">10 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Tom i Jerry: Przygoda w muzeum" href="/index.php/repertoire.html?id=11858">Tom i Jerry: Przygoda w muzeum </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">10 czerwca 2026</strong>
			<br><span class="hour">godz. 13:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Tom i Jerry: Przygoda w muzeum" href="/index.php/repertoire.html?id=11858">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Mandalorian i Grogu - 2D napisy" href="/index.php/repertoire.html?id=11861">Mandalorian i Grogu - 2D napisy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">10 czerwca 2026</strong>
			<br><span class="hour">godz. 15:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Mandalorian i Grogu - 2D napisy" href="/index.php/repertoire.html?id=11861">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - napisy" href="/index.php/repertoire.html?id=11873">Straszny film - napisy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">10 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - napisy" href="/index.php/repertoire.html?id=11873">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Zawodowcy" href="/index.php/repertoire.html?id=11868">Zawodowcy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">10 czerwca 2026</strong>
			<br><span class="hour">godz. 20:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Zawodowcy" href="/index.php/repertoire.html?id=11868">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">11 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Tom i Jerry: Przygoda w muzeum" href="/index.php/repertoire.html?id=11859">Tom i Jerry: Przygoda w muzeum </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">11 czerwca 2026</strong>
			<br><span class="hour">godz. 13:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Tom i Jerry: Przygoda w muzeum" href="/index.php/repertoire.html?id=11859">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Mandalorian i Grogu - 3D dubbing" href="/index.php/repertoire.html?id=11864">Mandalorian i Grogu - 3D dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">11 czerwca 2026</strong>
			<br><span class="hour">godz. 15:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Mandalorian i Grogu - 3D dubbing" href="/index.php/repertoire.html?id=11864">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - napisy" href="/index.php/repertoire.html?id=11874">Straszny film - napisy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">11 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - napisy" href="/index.php/repertoire.html?id=11874">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Zawodowcy" href="/index.php/repertoire.html?id=11869">Zawodowcy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">11 czerwca 2026</strong>
			<br><span class="hour">godz. 20:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Zawodowcy" href="/index.php/repertoire.html?id=11869">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">12 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11880">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">12 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11880">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11881">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">12 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11881">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11875">Straszny film - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">12 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11875">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11890">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">12 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11890">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">13 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11882">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">13 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11882">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11883">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">13 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11883">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11876">Straszny film - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">13 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11876">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11891">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">13 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11891">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">14 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11884">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">14 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11884">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11885">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">14 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11885">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11877">Straszny film - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">14 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11877">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11892">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">14 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11892">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">17 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11886">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">17 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11886">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11887">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">17 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11887">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11878">Straszny film - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">17 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11878">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11893">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">17 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11893">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">18 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11888">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">18 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11888">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11889">Drzewo magii - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">18 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Drzewo magii - dubbing" href="/index.php/repertoire.html?id=11889">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11879">Straszny film - dubbing </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">18 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Straszny film - dubbing" href="/index.php/repertoire.html?id=11879">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11894">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">18 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11894">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">19 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11900">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">19 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11900">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11901">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">19 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11901">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11902">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">19 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11902">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11895">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">19 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11895">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">20 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Sobota z teatrem dla dzieci - Czerwony Kapturek" href="/index.php/repertoire.html?id=11854">Sobota z teatrem dla dzieci - Czerwony Kapturek </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">20 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Sobota z teatrem dla dzieci - Czerwony Kapturek" href="/index.php/repertoire.html?id=11854">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11903">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">20 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11903">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11896">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">20 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11896">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">21 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11904">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">21 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11904">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11905">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">21 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11905">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11906">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">21 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11906">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11897">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">21 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11897">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">24 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11907">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">24 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11907">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11908">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">24 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11908">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11909">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">24 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11909">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11898">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">24 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11898">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">25 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11910">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">25 czerwca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11910">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11911">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">25 czerwca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11911">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11912">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">25 czerwca 2026</strong>
			<br><span class="hour">godz. 18:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11912">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11899">Dzień objawienia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">25 czerwca 2026</strong>
			<br><span class="hour">godz. 19:50</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Dzień objawienia" href="/index.php/repertoire.html?id=11899">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">26 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11913">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">26 czerwca 2026</strong>
			<br><span class="hour">godz. 14:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11913">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11914">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">26 czerwca 2026</strong>
			<br><span class="hour">godz. 16:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11914">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11933">Robin Hood: Koniec legendy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">26 czerwca 2026</strong>
			<br><span class="hour">godz. 18:15</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11933">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11938">Backrooms. Bez wyjścia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">26 czerwca 2026</strong>
			<br><span class="hour">godz. 20:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11938">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">27 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11915">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">27 czerwca 2026</strong>
			<br><span class="hour">godz. 14:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11915">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11916">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">27 czerwca 2026</strong>
			<br><span class="hour">godz. 16:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11916">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11939">Backrooms. Bez wyjścia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">27 czerwca 2026</strong>
			<br><span class="hour">godz. 18:15</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11939">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11934">Robin Hood: Koniec legendy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">27 czerwca 2026</strong>
			<br><span class="hour">godz. 20:20</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11934">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">28 czerwca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11917">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">28 czerwca 2026</strong>
			<br><span class="hour">godz. 14:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11917">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11918">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">28 czerwca 2026</strong>
			<br><span class="hour">godz. 16:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11918">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11935">Robin Hood: Koniec legendy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">28 czerwca 2026</strong>
			<br><span class="hour">godz. 18:15</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11935">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11940">Backrooms. Bez wyjścia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">28 czerwca 2026</strong>
			<br><span class="hour">godz. 20:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11940">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">1 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11919">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">1 lipca 2026</strong>
			<br><span class="hour">godz. 14:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11919">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11920">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">1 lipca 2026</strong>
			<br><span class="hour">godz. 16:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11920">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11941">Backrooms. Bez wyjścia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">1 lipca 2026</strong>
			<br><span class="hour">godz. 18:15</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11941">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11936">Robin Hood: Koniec legendy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">1 lipca 2026</strong>
			<br><span class="hour">godz. 20:20</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11936">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">2 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11921">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">2 lipca 2026</strong>
			<br><span class="hour">godz. 14:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11921">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11922">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">2 lipca 2026</strong>
			<br><span class="hour">godz. 16:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11922">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11937">Robin Hood: Koniec legendy </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">2 lipca 2026</strong>
			<br><span class="hour">godz. 18:15</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Robin Hood: Koniec legendy" href="/index.php/repertoire.html?id=11937">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11942">Backrooms. Bez wyjścia </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">2 lipca 2026</strong>
			<br><span class="hour">godz. 20:30</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Backrooms. Bez wyjścia" href="/index.php/repertoire.html?id=11942">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">3 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11923">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">3 lipca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11923">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11924">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">3 lipca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11924">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">4 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11925">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">4 lipca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11925">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11926">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">4 lipca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11926">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">5 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11927">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">5 lipca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11927">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11928">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">5 lipca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11928">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">8 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11929">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">8 lipca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11929">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11930">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">8 lipca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11930">kup bilet</a>		</td>
	</tr>
			
	<tr><td colspan="4" style="color: #943142;font-size: 12px;font-weight: bold;background: #e4dadc;">9 lipca 2026</td></tr>		<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11931">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">9 lipca 2026</strong>
			<br><span class="hour">godz. 14:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11931">kup bilet</a>		</td>
	</tr>
			
			<tr >
		<td class="title">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11932">Toy Story 5 </a>		</td>
		<td class="location">
			<span class="city">Płońsk</span><br/><span class="name">Sala widowiskowo-kinowa</span>
		</td>
		<td class="date">		
			<span class="day">9 lipca 2026</strong>
			<br><span class="hour">godz. 16:00</span>
		</td>
		<td class="link">
			<a title="Kup bilet - Toy Story 5" href="/index.php/repertoire.html?id=11932">kup bilet</a>		</td>
	</tr>
	</table>
<!--
<table id="rep3" class="table_info tbl_repertoire">
<tr>
	<td style="font-size: 20px;font-weight: bold;color: #890a3c;" colspan="4">Koncerty</td>
</tr>
<tr>
	<th class="tl"></th>
	<th></th>
	<th></th>
	<th class="tr"></th>
</tr>

		<tr >
		<td class="title">
					</td>
		<td class="location">
			<span class="city"></span><br/><span class="name"></span>
		</td>
		<td class="date">		
			<span class="day"></strong>
			<br><span class="hour">godz. </span>
		</td>
		<td class="link">
					</td>
	</tr>
	</table>

<table id="rep4" class="table_info tbl_repertoire">
<tr>
	<td style="font-size: 20px;font-weight: bold;color: #890a3c;" colspan="4">Spektakle</td>
</tr>
<tr>
	<th class="tl"></th>
	<th></th>
	<th></th>
	<th class="tr"></th>
</tr>

		<tr >
		<td class="title">
					</td>
		<td class="location">
			<span class="city"></span><br/><span class="name"></span>
		</td>
		<td class="date">		
			<span class="day"></strong>
			<br><span class="hour">godz. </span>
		</td>
		<td class="link">
					</td>
	</tr>
	</table>

<table id="rep5" class="table_info tbl_repertoire">
<tr>
	<td style="font-size: 20px;font-weight: bold;color: #890a3c;" colspan="4">Inne</td>
</tr>
<tr>
	<th class="tl"></th>
	<th></th>
	<th></th>
	<th class="tr"></th>
</tr>
			<tr >
		<td class="title">
					</td>
		<td class="location">
			<span class="city"></span><br/><span class="name"></span>
		</td>
		<td class="date">		
			<span class="day"></strong>
			<br><span class="hour">godz. </span>
		</td>
		<td class="link">
					</td>
	</tr>
	</table>
-->
<script type="text/javascript">	

	val5 = ;
	if(val5 == 0) {$('#rep5').hide();}

</script>

<!-- ,'title="'.__('kup bilet').' - '.$repertoire->getTitle().'"' -->
</div>
            <script type="text/javascript">
              scrolluj = true;

              floatingBox = function() {
                var $floatingbox = $('#moving_cart');

                if ($('#body').length > 0) {
                  if ($(window).scrollTop() < 36) {
                    $('#hide_cart').css('visibility', "hidden");
                  }
                  $(window).scroll(function() {
                    if (scrolluj == true) {
                      //console.log(scrolluj);
                      if ($floatingbox.length > 0) {
                        if ($(window).scrollTop() > 36) {
                          $('#moving_cart').addClass('cart_move');
                          $('#moving_cart').removeClass('cart_top');
                          $('#cart_section').addClass('cart_opacity');
                          $('#cart_section').removeClass('cart_no_opacity');
                          $('#hide_cart').css('visibility', "visible");


                        } else {
                          $('#moving_cart').removeClass('cart_move');
                          $('#moving_cart').addClass('cart_top');
                          $('#cart_section').addClass('cart_no_opacity');
                          $('#cart_section').removeClass('cart_opacity');
                          $('#hide_cart').css('visibility', "hidden");
                        }
                      }
                    }
                  });
                }

              }

              $("#hide_cart").on("click", function(e) {
                e.preventDefault();
                $('#moving_cart').removeClass('cart_move');
                $('#moving_cart').addClass('cart_top');
                $('#cart_section').addClass('cart_no_opacity');
                $('#cart_section').removeClass('cart_opacity');
                $('#hide_cart').css('display', "none");
                scrolluj = false;
              });

              new floatingBox();
              function  sbCartDetailsAnimate() {
                var sb_content = $("#sb_cart_content")
                var sb_content_label = $("#sb_cart_detail_label");
                if (sb_content.css('display') == 'none') {
                  var pos = $("#sb_cart").position();
                  var pos = $("#sb_cart").position();
                  var height = $("#sb_cart").height();
                  sb_content.css('position', 'absolute')
                  sb_content.css('left', pos.left + 300)
                  sb_content.css('top', (pos.top + height + 100))
                  sb_content.css('z-index', '1000')
                  sb_content.slideDown(500)
                  sb_content_label.text("Ukryj szczegóły koszyka");
                } else {
                  sb_content.slideUp(500);
                  sb_content_label.text("pokaż szczegóły koszyka");
                }
              }

              function animateCartError(error) {
                if ($("#sb_cart_content").css('display') != 'none') {
                  $("#sb_cart_content").slideUp(500);
                  $("#sb_cart_detail_label").text("pokaż szczegóły koszyka");
                }
                var w = $(window).width();
                var h = $(window).height();
                $('body').append($('<div id="loader" style="background: white; position: absolute; top: 0px; left: 0px; width: ' + w + 'px; height: ' + (h + 500) + 'px; text-align: center;"></div>').css('opacity', 0.5)).append('<div id="loader_logo" class="ui-state-error" style="width: 500px; position: absolute; top: ' + (h / 2) + 'px; left: ' + (w / 2 - 250) + 'px; font-weight: bold; text-align: center;">' + error + '</div>');
                setTimeout('$("#loader,#loader_logo").remove()', 5000);
              }

              $(function() {

                $("#loader").live('click', function() {
                  $("#loader,#loader_logo").remove()
                })
                // pobranie koszyka po odswiezeniu strony
                $("#sb_cart_header").click(function() {
                  sbCartDetailsAnimate();
                });
                cart.onLoaded(function() {
                  for (e in this._items) { // przelecenie po kazdym elemencie z koszyka
                    var item = this._items[e];
                    buildCartItem(item);
                  }
                  //console.log(this.count());
                  if (this.count() > 0) {
                    $("#cart_buy_button").show();
                    $("#sb_cart_detail_label").show();
                  } else {
                    $("#cart_buy_button").hide();
                    $("#sb_cart_detail_label").hide();
                  }
                });
                cart.onLoading(function() {
                  $('.details_content').text('');
                });
                // obsluga dodawania elemntu do koszyka
                cart.addOption('onCartAdd', function(Event, item) {

                  $('.details_content').append($('<div>dodawanie produktu do koszyka</div>').attr('id', this.getUID()));
                  $("#cart_buy_button").show();
                  $("#sb_cart_detail_label").show();
                });
                // obsluga usuwania elementu z koszyka
                cart.addOption('onCartDel', function(Event, item) {
                  if (this.TYPE == 'CartItemPlace') {
                    if (window.preview != undefined) {
                      var element = preview.getPlace('place[id=' + this.getPlaceId() + ']');
                      if (element.length > 0) {
                        var repertoire_id = element.parent().attr('repertoire_id');
                        if (repertoire_id == this.getRepertoireId()) {
                          $('#path_' + element.attr('id')).trigger('item_deselect');
                        }
                      }
                    }
                    $('#' + this.getUID()).remove();
                  } else if (this.TYPE == 'CartItemRepertoire') {
                    $('#' + this.getUID()).remove();
                  }
                  $('#sb_cart .worth').text(this.getCart().getWorth() + ' zł');
                  //PS dane do externala
                  $('.cart_buy_button').attr('href', "/index.php/order/summary.html?external[quantity]=" + this.getCart().count() + "&external[worth]=" + this.getCart().getWorth());
                  if (this.getCart().count() <= 0) {
                    $("#cart_buy_button").hide();
                    $("#sb_cart_detail_label").hide();
                    $('#sb_cart .count').text('0');
                    $("#sb_cart_content").css('display', "none");
                    $("#sb_cart_detail_label").text("pokaż szczegóły koszyka");
                  } else {
                    $('#sb_cart .count').text(this.getCart().count());
                  }
                });
                // obsluga dodawania elemntu do koszyka
                cart.addOption('onCartLoaded', function(Event) {
                  $('#' + this.getUID()).removeClass('ui-state-disabled');
                  buildCartItem(this);
                });
                // obsluga ladowania (podczas dodawania lub usuwania)
                cart.addOption('onCartLoading', function(Event) {
                  $('#sb_cart .count').text('Aktualizacja koszyka...');
                  $('#sb_cart .worth').text('Zobacz szczegóły');
                  $('#' + this.getUID()).addClass('ui-state-disabled');
                });
                // obsluga ladowania (podczas dodawania lub usuwania)
                cart.addOption('onCartError', function(Event, code, error, method) {
                  animateCartError(error);
                  $('#sb_cart .count').text(this.getCart().count());
                  $('#sb_cart .worth').text(this.getCart().getWorth() + ' zł');
                  //PS dane do externala
                  $('.cart_buy_button').attr('href', "/index.php/order/summary.html?");
                  $('#' + this.getUID()).css('margin', '1px').addClass('ui-state-error').removeClass('ui-state-disabled');
                  if (method == 'add') {
                    $('#' + this.getUID()).text(error);
                    if (this.TYPE == 'CartItemPlace') {
                      if (window.preview != undefined) {
                        var element = preview.getPlace('place[id=' + this.getPlaceId() + ']');
                        if (element.length > 0) {
                          var repertoire_id = element.parent().attr('repertoire_id');
                          if (repertoire_id == this.getRepertoireId()) {
                            $('#path_' + element.attr('id')).trigger('item_deselect');
                          }
                        }
                      }
                    } else if (this.TYPE == 'CartItemRepertoire') {

                    }
                    $('#' + this.getUID()).delay(10000).slideUp(400);
                  } else if (method == 'del') {
                    $('#' + this.getUID()).append($('<div style="color: #FFFFFF;">' + error + '</div>').delay(10000).slideUp(400));
                    setTimeout('$(".ui-state-error").remove()', 5000);
                  }
                });
              });
              function buildCartItem(item) {
                if (item.TYPE == 'CartItemPlace') {
                  if (window.preview != undefined) {
                    var element = preview.getPlace('place[id=' + item.getPlaceId() + ']');
                    if (element.length > 0) {
                      var repertoire_id = element.parent().attr('repertoire_id');
                      if (repertoire_id == item.getRepertoireId()) {
                        $('#path_' + element.attr('id')).trigger('item_select');
                      }
                    }
                  }
                } else if (this.TYPE == 'CartItemRepertoire') {

                }
                $('#' + item.getUID()).text('').css('margin', '1px').css('padding', '2px').addClass('ui-widget-content');
                var div_price = $('<div style="float: right; margin-left: 10px;font-weight: bold;">' + item.getWorth() + ' zł<span id="del_' + item.getUID() + '" style="margin-left:10px; margin-right: 5px; color: red; cursor: pointer">Usun</span></div>');
                $('#' + item.getUID()).append(div_price);
                var div = null;
                if (item.TYPE == 'CartItemPlace') {
                  div = $('<div style="float: left;"><strong>' + item.getInfo('event_name') + '</strong><br/>Ilość: <strong>' + item.getQuantity() + '</strong> - ' + (item.getInfo('section').length > 0 ? 'sektor: <strong>' + item.getInfo('section') + '</strong>' : '') + ' ' + (item.getInfo('row').length > 0 ? 'rzad: <strong>' + item.getInfo('row') + '</strong> miejsce: <strong>' + item.getInfo('number') + '</strong></div>' : '<strong>miejsca nienumerowane</strong>') + '</div>');

                  //+' rząd: <strong>'+item.getInfo('row')+'</strong> miejsce: <strong>'+item.getInfo('number')+'</strong></div>')

                  $('#' + item.getUID()).append(div);
                  div = $('<div style="float: left; margin-left: 2px">' + item.getInfo('event_date') + '</div>');
                  $('#' + item.getUID()).append(div);
                } else if (item.TYPE == 'CartItemRepertoire') {
                  div = $('<div style="float: left;"><strong>' + item.getInfo('event_name') + '</strong> Ilość: <strong>' + item.getQuantity() + '</strong><br/>' + (item.getInfo('section').length > 0 ? 'sektor: <strong>' + item.getInfo('section') + '</strong>' : '') + ' ' + item.getName() + ' - miejsca nienumerowane</div>');
                  $('#' + item.getUID()).append(div);
                  div = $('<div style="float: left; margin-left: 2px">' + item.getInfo('event_date') + '</div>');
                  $('#' + item.getUID()).append(div);
                } else if (item.TYPE == 'CartItem') {

                }
                //div = $('');
                //$('#'+item.getUID()).append(div);
                $('#' + item.getUID()).append($('<div style="clear: both;"></div>'));
                div_price.position({
                  of: $('#' + item.getUID()),
                  my: 'right center',
                  at: 'right center'
                });
                $('#sb_cart .count').text(item.getCart().count());
                $('#sb_cart .worth').text(item.getCart().getWorth() + ' zł');
                //PS dane do externala
                $('.cart_buy_button').attr('href', "/index.php/order/summary.html?external[quantity]=" + item.getCart().count() + "&external[worth]=" + item.getCart().getWorth());
                $('#del_' + item.getUID()).click(function() {
                  item.del();
                });
              }
            </script>
                          <script type="text/javascript">
                $(function() {
                  cart.load();
                });
              </script>
  
        </div>
        <div class="footer">
        <table class="params">
	<tr>
		<td style="width: 26px;">
			<img style="float: left;valign: middle;margin: 2px 5px;" src="/images/ikona16x16.png" /> 
		</td>
		<td style="text-align: left;">
			<span class="visual"><a target="_blank" href="http://www.systembiletowy.pl">visualTicket</a> &copy; 2005-2026 by <a target="_blank" href="http://www.visualnet.pl">visualnet.pl</a>
		</td>
		<td style="text-align: right;font-size: 9px;">
			Theme: ckp<br/>
			Language: pl</span>
		</td>
	</tr>
	<tr>
		<td colspan="3" style="text-align: center;">
			<strong>Płatności realizowane za pomocą serwisu</strong><br/><br/>
			<img style="width: 120px;" src="/images/przelewy24.jpg" /><br/>
			<img style="width: 120px;" src="/images/thawte_secured.jpg" /><br/>
		</td>
	</tr>
</table>

	            
        </div>
      </div>
    </div>
      
        <!-- monit area --> 
        
        <noscript>
                      <div class="dialog-background">&nbsp;</div>

          <div class="dialog-root">

              <div class="dialog">
                  <p style="text-align:center;">Musisz włączyć obsługę JavaScript</p>
              </div>
          </div>        </noscript> 
      
        <span id="visualnet-monit-cookies" style="display: none;">
                      <div class="dialog-background">&nbsp;</div>

          <div class="dialog-root">

              <div class="dialog">
                  <p style="text-align:center;">Proszę włączyć obsługę ciasteczek</p>
              </div>
          </div>            
        </span>    
      
        <div id="visualnet-dialog" style="display: none;"></div>
        
        <script>

            $(document).ready(function () {

                Visualnet.utils = new Visualnet.Utils(); 

                $("#visualnet-monit-cookies").toggle(Visualnet.utils.isCookiesEnabled());
                $("#visualnet-monit-browser").toggle(Visualnet.utils.checkIfBrowser(Visualnet.utils.browser.ie, 8, "="));
            
            });
            
        </script>  
        
        <!-- monit area / --> 
	<div id="sb_cart_content" class="details_content" style="display:none"></div> 
  </body>
</html>
