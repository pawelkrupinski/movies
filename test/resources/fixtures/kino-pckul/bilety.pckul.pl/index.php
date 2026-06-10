<style>
   .description {
    height: 75px;
    overflow: hidden;
    transition: height 0.5s ease;
}

.description.expanded {
    height: auto;
}

.toggleButton {
    cursor: pointer;
    background: none;
    border: 1px solid;
	border-radius: 5px;
    font-size: 14px;
    margin-top: 10px;
    /* float: right; */
    padding: 3px 15px;
    min-width: 100px;
    background-color: #000;
    color: #fff;
}

</style>

		<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html" />
<meta http-equiv="Cache-Control" content="no-store, no-cache, must-revalidate, post-check=0, pre-check=0" />
    	
	<!-- metas external -->
	<meta name="theme-color" content="#a7a7a7">
	
	<link rel="icon" type="image/png" href="/images/pck2/favicon.png"><!-- Major Browsers -->
	<!--[if IE]><link rel="SHORTCUT ICON" href="/favicon_vt.ico"/><![endif]--><!-- Internet Explorer--> 
	
	<meta name="viewport" content="width=device-width, initial-scale=1">		
	<meta http-equiv="content-type" content="text/html" />
	<meta http-equiv="Cache-Control" content="no-store, no-cache, must-revalidate, post-check=0, pre-check=0" />	
    	
	<meta property="og:image" content="https://bilety.pckul.pl/images/pck2/favicon.png" />
	
	<!-- end metas external -->
	
	<!-- title external -->
	
		
					<title>System Sprzedaży biletów - Pszczyńskie Centrum Kultury - system biletowy</title>
		<meta name="description" content="Sprzedaż biletów na wydarzenia - kup bilet - Pszczyńskie Centrum Kultury" />
		<meta name="title" content="System Sprzedaży biletów - Pszczyńskie Centrum Kultury - system biletowy" />
				
	
	<!-- end title external -->	
	
    <link rel="stylesheet" type="text/css" media="screen" href="/css/pck2/system_portal.css" />
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
 
	
	<link href='https://fonts.googleapis.com/css?family=Titillium+Web' rel='stylesheet' type='text/css'>
	<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7" crossorigin="anonymous">
	<link href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css" rel="stylesheet" integrity="sha384-T8Gy5hrqNKT+hzMclPo118YTQO6cYprQmhrYwIiQ/3axmI1hQomh7Ud2hPOy8SP1" crossorigin="anonymous">
	<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.1.0/css/all.css" integrity="sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt" crossorigin="anonymous">
    <script type="text/javascript">
        function postIframeMessage(){ 

            //var POST_DOMAIN = "http://www.chck.pl/";
            //var body = document.body, html = document.documentElement; 
            //height = Math.max(body.scrollHeight, body.offsetHeight, html.clientHeight, html.scrollHeight, html.offsetHeight); 

           // if (parent.postMessage) {
               // parent.postMessage(height, POST_DOMAIN);
            //}
			
			////$('html, body', window.parent.document).animate({scrollTop:0}, 'slow');
        }
    </script>


      <!-- Google Tag Manager -->
      <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
                  new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
              j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
              'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
          })(window,document,'script','dataLayer','GTM-PVFWZK7');</script>
      <!-- End Google Tag Manager -->
      <!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=UA-169305990-1"></script>
      <script>
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          
          gtag('config', 'UA-169305990-1');
      </script>


  </head>
  <body onload="postIframeMessage();" id="body">
	 	 		<div class="navigation" onclick="window.location='https://pckul.pl/';">
	<div class="col-xs-12 col-sm-12 col-md-12 col-lg-12">
		<div class="main-logo"><a target="_blank" href="https://pckul.pl/"><img src="/images/pck2/pck-logo.png"></a></div>
		<div class="main-lng" style="">
			<a class="language-select active" href="?culture=pl&id=" title="" >PL</a>
			<a class="language-select " href="?culture=en&id=" title="">EN</a>
		</div>
		<!-- 			<div class="cart-small">
			<a href="/order/summary.html?external">	<i class="fas fa-shopping-cart" style="cursor: pointer;"></i></a>
				<span class="count">0</span>
			</div>
						<div class="rfilters">
			dziś: środa, 10 czerwca 2026<br/> 
			<span onClick="showday('2026-06-10')" class="rfiltr today"><i class="fas fa-search"></i> dziś</span>
			<span onClick="showday('2026-06-11')" class="rfiltr tommorow"><i class="fas fa-search"></i> jutro</span>
			<span onClick="showday('2026-06-12')" class="rfiltr datommorow"><i class="fas fa-search"></i> pojutrze</span>
			<span onClick="$('.repertoire-once').slideDown();$('.no-repertoire').slideUp();" class="rfiltr all"><i class="fas fa-binoculars"></i> wszystko</span>
		</div>			
				 -->
		
	</div>
</div>
<div class="navigation2" onclick="window.location='https://pckul.pl/';">
	<div class="main-logo small"><a target="_blank" href="https://pckul.pl/"><img src="/images/pck2/pck-logo.png"></a></div>
			<div class="cart-small">
		<a href="/order/summary.html?external">	<i class="fas fa-shopping-cart"></i></a>
			<span class="count">0</span>
		</div>
	</div>
	  
	       
	  <div id="container">
        	
		
		<div style="margin: 20px auto; text-align: center;color: red;display: none;" id="visualnet-monit-browser" class="browser-monit">
            Wymagana przeglądarka Chrome, Firefox lub Opera w najnowszej wersji. Prosimy zaktualizuj swoją przeglądarkę i zapraszamy ponownie.
        </div>
                    
        <script type="text/javascript">
  var cart = null;
  $(function () {
    cart = new Cart('https://bilety.pckul.pl/service.php');
    cart.session = '5emlpnleh5l7og8h5bk6sdbue2';
  });
</script>
	
		 
          		  
          <div class="">	<!-- <div class="hidden-xs hidden-sm hidden-md col-lg-1"></div> -->
	<div class="col-xs-12 col-sm-12 col-md-12 col-lg-12" style="padding: 0;">

				<div class="nav-box">
			<!-- <div class="btn-calendar" onclick="myFunction()">Kalendarz</div> -->
							<div class="cart-small">
					<a href="/order/summary.html?external"> <i class="fas fa-shopping-cart" style="cursor: pointer;"></i></a>
					<span class="count">0</span>
				</div>
										<div class="rfilters">
					<span style="margin-right:
        4px; color: #FFCC00;">dziś:</span> środa, 10 czerwca 2026<br />
					<!-- <span onClick="showday('2026-06-10')" class="rfiltr today"><i class="fas fa-search"></i> dziś</span>
			<span onClick="showday('2026-06-11')" class="rfiltr tommorow"><i class="fas fa-search"></i> jutro</span>
			<span onClick="showday('2026-06-12')" class="rfiltr datommorow"><i class="fas fa-search"></i> pojutrze</span>
			<span onClick="$('.repertoire-once').slideDown();$('.no-repertoire').slideUp();" class="rfiltr all"><i class="fas fa-binoculars"></i> wszystko</span> -->

				</div>
				<div class="btn-calendar" onclick="toggleCalendar()">Kalendarz</div>
					</div>
		<div id="calendar" class="calendar" style="display: none;">
			<div class="col-xs-12 col-sm-12 col-md-12 col-lg-8 headline-box showdate">wybierz datę</div>
							<div class="row col-xs-12 month-days month-0" >
					<div class="col-xs-6 col-sm-6 col-md-1 col-lg-12 month-name">Czerwiec</div>
					<div class="col-xs-3 col-sm-2 col-md-1 col-lg-1 fa fa-arrow-left"  ></div>


					<div class="col-xs-3 col-sm-2 hidden-md hidden-lg fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-1').show();" ></div>

					<div class="calendar-days col-xs-12 col-sm-12 col-md-9 col-lg-10">
												<div class="calendar-weekday-row">
															<div class="calendar-weekday-head">Pn</div>
															<div class="calendar-weekday-head">Wt</div>
															<div class="calendar-weekday-head">Śr</div>
															<div class="calendar-weekday-head">Cz</div>
															<div class="calendar-weekday-head">Pt</div>
															<div class="calendar-weekday-head">So</div>
															<div class="calendar-weekday-head">Nd</div>
													</div>
						<div class="calendar-days-center">
																						<div class="calendar-day" data-date="2026-06-01" onclick="showday('2026-06-01')">Pn<br>01</div>
																							<div class="calendar-day" data-date="2026-06-02" onclick="showday('2026-06-02')">Wt<br>02</div>
																							<div class="calendar-day" data-date="2026-06-03" onclick="showday('2026-06-03')">Śr<br>03</div>
																							<div class="calendar-day" data-date="2026-06-04" onclick="showday('2026-06-04')">Cz<br>04</div>
																							<div class="calendar-day" data-date="2026-06-05" onclick="showday('2026-06-05')">Pt<br>05</div>
																							<div class="calendar-day" data-date="2026-06-06" onclick="showday('2026-06-06')">So<br>06</div>
																							<div class="calendar-day" data-date="2026-06-07" onclick="showday('2026-06-07')">Nd<br>07</div>
																							<div class="calendar-day" data-date="2026-06-08" onclick="showday('2026-06-08')">Pn<br>08</div>
																							<div class="calendar-day" data-date="2026-06-09" onclick="showday('2026-06-09')">Wt<br>09</div>
																							<div class="calendar-day" data-date="2026-06-10" onclick="showday('2026-06-10')">Śr<br>10</div>
																							<div class="calendar-day" data-date="2026-06-11" onclick="showday('2026-06-11')">Cz<br>11</div>
																							<div class="calendar-day" data-date="2026-06-12" onclick="showday('2026-06-12')">Pt<br>12</div>
																							<div class="calendar-day" data-date="2026-06-13" onclick="showday('2026-06-13')">So<br>13</div>
																							<div class="calendar-day" data-date="2026-06-14" onclick="showday('2026-06-14')">Nd<br>14</div>
																							<div class="calendar-day" data-date="2026-06-15" onclick="showday('2026-06-15')">Pn<br>15</div>
																							<div class="calendar-day" data-date="2026-06-16" onclick="showday('2026-06-16')">Wt<br>16</div>
																							<div class="calendar-day" data-date="2026-06-17" onclick="showday('2026-06-17')">Śr<br>17</div>
																							<div class="calendar-day" data-date="2026-06-18" onclick="showday('2026-06-18')">Cz<br>18</div>
																							<div class="calendar-day" data-date="2026-06-19" onclick="showday('2026-06-19')">Pt<br>19</div>
																							<div class="calendar-day" data-date="2026-06-20" onclick="showday('2026-06-20')">So<br>20</div>
																							<div class="calendar-day" data-date="2026-06-21" onclick="showday('2026-06-21')">Nd<br>21</div>
																							<div class="calendar-day" data-date="2026-06-22" onclick="showday('2026-06-22')">Pn<br>22</div>
																							<div class="calendar-day" data-date="2026-06-23" onclick="showday('2026-06-23')">Wt<br>23</div>
																							<div class="calendar-day" data-date="2026-06-24" onclick="showday('2026-06-24')">Śr<br>24</div>
																							<div class="calendar-day" data-date="2026-06-25" onclick="showday('2026-06-25')">Cz<br>25</div>
																							<div class="calendar-day" data-date="2026-06-26" onclick="showday('2026-06-26')">Pt<br>26</div>
																							<div class="calendar-day" data-date="2026-06-27" onclick="showday('2026-06-27')">So<br>27</div>
																							<div class="calendar-day" data-date="2026-06-28" onclick="showday('2026-06-28')">Nd<br>28</div>
																							<div class="calendar-day" data-date="2026-06-29" onclick="showday('2026-06-29')">Pn<br>29</div>
																							<div class="calendar-day" data-date="2026-06-30" onclick="showday('2026-06-30')">Wt<br>30</div>
																																					<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
													</div>
					</div>

					<div style="text-align: right;" class="hidden-xs hidden-sm col-md-1 col-lg-1 fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-1').show();" ></div>
				</div>
							<div class="row col-xs-12 month-days month-1" style="display: none" >
					<div class="col-xs-6 col-sm-6 col-md-1 col-lg-12 month-name">Lipiec</div>
					<div class="col-xs-3 col-sm-2 col-md-1 col-lg-1 fa fa-arrow-left"  onclick="$('.month-days').hide(); $('.month-0').show();" ></div>


					<div class="col-xs-3 col-sm-2 hidden-md hidden-lg fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-2').show();" ></div>

					<div class="calendar-days col-xs-12 col-sm-12 col-md-9 col-lg-10">
												<div class="calendar-weekday-row">
															<div class="calendar-weekday-head">Pn</div>
															<div class="calendar-weekday-head">Wt</div>
															<div class="calendar-weekday-head">Śr</div>
															<div class="calendar-weekday-head">Cz</div>
															<div class="calendar-weekday-head">Pt</div>
															<div class="calendar-weekday-head">So</div>
															<div class="calendar-weekday-head">Nd</div>
													</div>
						<div class="calendar-days-center">
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
																						<div class="calendar-day" data-date="2026-07-01" onclick="showday('2026-07-01')">Śr<br>01</div>
																							<div class="calendar-day" data-date="2026-07-02" onclick="showday('2026-07-02')">Cz<br>02</div>
																							<div class="calendar-day" data-date="2026-07-03" onclick="showday('2026-07-03')">Pt<br>03</div>
																							<div class="calendar-day" data-date="2026-07-04" onclick="showday('2026-07-04')">So<br>04</div>
																							<div class="calendar-day" data-date="2026-07-05" onclick="showday('2026-07-05')">Nd<br>05</div>
																							<div class="calendar-day" data-date="2026-07-06" onclick="showday('2026-07-06')">Pn<br>06</div>
																							<div class="calendar-day" data-date="2026-07-07" onclick="showday('2026-07-07')">Wt<br>07</div>
																							<div class="calendar-day" data-date="2026-07-08" onclick="showday('2026-07-08')">Śr<br>08</div>
																							<div class="calendar-day" data-date="2026-07-09" onclick="showday('2026-07-09')">Cz<br>09</div>
																							<div class="calendar-day" data-date="2026-07-10" onclick="showday('2026-07-10')">Pt<br>10</div>
																							<div class="calendar-day" data-date="2026-07-11" onclick="showday('2026-07-11')">So<br>11</div>
																							<div class="calendar-day" data-date="2026-07-12" onclick="showday('2026-07-12')">Nd<br>12</div>
																							<div class="calendar-day" data-date="2026-07-13" onclick="showday('2026-07-13')">Pn<br>13</div>
																							<div class="calendar-day" data-date="2026-07-14" onclick="showday('2026-07-14')">Wt<br>14</div>
																							<div class="calendar-day" data-date="2026-07-15" onclick="showday('2026-07-15')">Śr<br>15</div>
																							<div class="calendar-day" data-date="2026-07-16" onclick="showday('2026-07-16')">Cz<br>16</div>
																							<div class="calendar-day" data-date="2026-07-17" onclick="showday('2026-07-17')">Pt<br>17</div>
																							<div class="calendar-day" data-date="2026-07-18" onclick="showday('2026-07-18')">So<br>18</div>
																							<div class="calendar-day" data-date="2026-07-19" onclick="showday('2026-07-19')">Nd<br>19</div>
																							<div class="calendar-day" data-date="2026-07-20" onclick="showday('2026-07-20')">Pn<br>20</div>
																							<div class="calendar-day" data-date="2026-07-21" onclick="showday('2026-07-21')">Wt<br>21</div>
																							<div class="calendar-day" data-date="2026-07-22" onclick="showday('2026-07-22')">Śr<br>22</div>
																							<div class="calendar-day" data-date="2026-07-23" onclick="showday('2026-07-23')">Cz<br>23</div>
																							<div class="calendar-day" data-date="2026-07-24" onclick="showday('2026-07-24')">Pt<br>24</div>
																							<div class="calendar-day" data-date="2026-07-25" onclick="showday('2026-07-25')">So<br>25</div>
																							<div class="calendar-day" data-date="2026-07-26" onclick="showday('2026-07-26')">Nd<br>26</div>
																							<div class="calendar-day" data-date="2026-07-27" onclick="showday('2026-07-27')">Pn<br>27</div>
																							<div class="calendar-day" data-date="2026-07-28" onclick="showday('2026-07-28')">Wt<br>28</div>
																							<div class="calendar-day" data-date="2026-07-29" onclick="showday('2026-07-29')">Śr<br>29</div>
																							<div class="calendar-day" data-date="2026-07-30" onclick="showday('2026-07-30')">Cz<br>30</div>
																							<div class="calendar-day" data-date="2026-07-31" onclick="showday('2026-07-31')">Pt<br>31</div>
																																					<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
													</div>
					</div>

					<div style="text-align: right;" class="hidden-xs hidden-sm col-md-1 col-lg-1 fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-2').show();" ></div>
				</div>
							<div class="row col-xs-12 month-days month-2" style="display: none" >
					<div class="col-xs-6 col-sm-6 col-md-1 col-lg-12 month-name">Sierpień</div>
					<div class="col-xs-3 col-sm-2 col-md-1 col-lg-1 fa fa-arrow-left"  onclick="$('.month-days').hide(); $('.month-1').show();" ></div>


					<div class="col-xs-3 col-sm-2 hidden-md hidden-lg fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-3').show();" ></div>

					<div class="calendar-days col-xs-12 col-sm-12 col-md-9 col-lg-10">
												<div class="calendar-weekday-row">
															<div class="calendar-weekday-head">Pn</div>
															<div class="calendar-weekday-head">Wt</div>
															<div class="calendar-weekday-head">Śr</div>
															<div class="calendar-weekday-head">Cz</div>
															<div class="calendar-weekday-head">Pt</div>
															<div class="calendar-weekday-head">So</div>
															<div class="calendar-weekday-head">Nd</div>
													</div>
						<div class="calendar-days-center">
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
																						<div class="calendar-day" data-date="2026-08-01" onclick="showday('2026-08-01')">So<br>01</div>
																							<div class="calendar-day" data-date="2026-08-02" onclick="showday('2026-08-02')">Nd<br>02</div>
																							<div class="calendar-day" data-date="2026-08-03" onclick="showday('2026-08-03')">Pn<br>03</div>
																							<div class="calendar-day" data-date="2026-08-04" onclick="showday('2026-08-04')">Wt<br>04</div>
																							<div class="calendar-day" data-date="2026-08-05" onclick="showday('2026-08-05')">Śr<br>05</div>
																							<div class="calendar-day" data-date="2026-08-06" onclick="showday('2026-08-06')">Cz<br>06</div>
																							<div class="calendar-day" data-date="2026-08-07" onclick="showday('2026-08-07')">Pt<br>07</div>
																							<div class="calendar-day" data-date="2026-08-08" onclick="showday('2026-08-08')">So<br>08</div>
																							<div class="calendar-day" data-date="2026-08-09" onclick="showday('2026-08-09')">Nd<br>09</div>
																							<div class="calendar-day" data-date="2026-08-10" onclick="showday('2026-08-10')">Pn<br>10</div>
																							<div class="calendar-day" data-date="2026-08-11" onclick="showday('2026-08-11')">Wt<br>11</div>
																							<div class="calendar-day" data-date="2026-08-12" onclick="showday('2026-08-12')">Śr<br>12</div>
																							<div class="calendar-day" data-date="2026-08-13" onclick="showday('2026-08-13')">Cz<br>13</div>
																							<div class="calendar-day" data-date="2026-08-14" onclick="showday('2026-08-14')">Pt<br>14</div>
																							<div class="calendar-day" data-date="2026-08-15" onclick="showday('2026-08-15')">So<br>15</div>
																							<div class="calendar-day" data-date="2026-08-16" onclick="showday('2026-08-16')">Nd<br>16</div>
																							<div class="calendar-day" data-date="2026-08-17" onclick="showday('2026-08-17')">Pn<br>17</div>
																							<div class="calendar-day" data-date="2026-08-18" onclick="showday('2026-08-18')">Wt<br>18</div>
																							<div class="calendar-day" data-date="2026-08-19" onclick="showday('2026-08-19')">Śr<br>19</div>
																							<div class="calendar-day" data-date="2026-08-20" onclick="showday('2026-08-20')">Cz<br>20</div>
																							<div class="calendar-day" data-date="2026-08-21" onclick="showday('2026-08-21')">Pt<br>21</div>
																							<div class="calendar-day" data-date="2026-08-22" onclick="showday('2026-08-22')">So<br>22</div>
																							<div class="calendar-day" data-date="2026-08-23" onclick="showday('2026-08-23')">Nd<br>23</div>
																							<div class="calendar-day" data-date="2026-08-24" onclick="showday('2026-08-24')">Pn<br>24</div>
																							<div class="calendar-day" data-date="2026-08-25" onclick="showday('2026-08-25')">Wt<br>25</div>
																							<div class="calendar-day" data-date="2026-08-26" onclick="showday('2026-08-26')">Śr<br>26</div>
																							<div class="calendar-day" data-date="2026-08-27" onclick="showday('2026-08-27')">Cz<br>27</div>
																							<div class="calendar-day" data-date="2026-08-28" onclick="showday('2026-08-28')">Pt<br>28</div>
																							<div class="calendar-day" data-date="2026-08-29" onclick="showday('2026-08-29')">So<br>29</div>
																							<div class="calendar-day" data-date="2026-08-30" onclick="showday('2026-08-30')">Nd<br>30</div>
																							<div class="calendar-day" data-date="2026-08-31" onclick="showday('2026-08-31')">Pn<br>31</div>
																																					<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
													</div>
					</div>

					<div style="text-align: right;" class="hidden-xs hidden-sm col-md-1 col-lg-1 fa fa-arrow-right"  onclick="$('.month-days').hide(); $('.month-3').show();" ></div>
				</div>
							<div class="row col-xs-12 month-days month-3" style="display: none" >
					<div class="col-xs-6 col-sm-6 col-md-1 col-lg-12 month-name">Wrzesień</div>
					<div class="col-xs-3 col-sm-2 col-md-1 col-lg-1 fa fa-arrow-left"  onclick="$('.month-days').hide(); $('.month-2').show();" ></div>


					<div class="col-xs-3 col-sm-2 hidden-md hidden-lg fa fa-arrow-right"  ></div>

					<div class="calendar-days col-xs-12 col-sm-12 col-md-9 col-lg-10">
												<div class="calendar-weekday-row">
															<div class="calendar-weekday-head">Pn</div>
															<div class="calendar-weekday-head">Wt</div>
															<div class="calendar-weekday-head">Śr</div>
															<div class="calendar-weekday-head">Cz</div>
															<div class="calendar-weekday-head">Pt</div>
															<div class="calendar-weekday-head">So</div>
															<div class="calendar-weekday-head">Nd</div>
													</div>
						<div class="calendar-days-center">
															<div class="calendar-day-placeholder"></div>
																						<div class="calendar-day" data-date="2026-09-01" onclick="showday('2026-09-01')">Wt<br>01</div>
																							<div class="calendar-day" data-date="2026-09-02" onclick="showday('2026-09-02')">Śr<br>02</div>
																							<div class="calendar-day" data-date="2026-09-03" onclick="showday('2026-09-03')">Cz<br>03</div>
																							<div class="calendar-day" data-date="2026-09-04" onclick="showday('2026-09-04')">Pt<br>04</div>
																							<div class="calendar-day" data-date="2026-09-05" onclick="showday('2026-09-05')">So<br>05</div>
																							<div class="calendar-day" data-date="2026-09-06" onclick="showday('2026-09-06')">Nd<br>06</div>
																							<div class="calendar-day" data-date="2026-09-07" onclick="showday('2026-09-07')">Pn<br>07</div>
																							<div class="calendar-day" data-date="2026-09-08" onclick="showday('2026-09-08')">Wt<br>08</div>
																							<div class="calendar-day" data-date="2026-09-09" onclick="showday('2026-09-09')">Śr<br>09</div>
																							<div class="calendar-day" data-date="2026-09-10" onclick="showday('2026-09-10')">Cz<br>10</div>
																							<div class="calendar-day" data-date="2026-09-11" onclick="showday('2026-09-11')">Pt<br>11</div>
																							<div class="calendar-day" data-date="2026-09-12" onclick="showday('2026-09-12')">So<br>12</div>
																							<div class="calendar-day" data-date="2026-09-13" onclick="showday('2026-09-13')">Nd<br>13</div>
																							<div class="calendar-day" data-date="2026-09-14" onclick="showday('2026-09-14')">Pn<br>14</div>
																							<div class="calendar-day" data-date="2026-09-15" onclick="showday('2026-09-15')">Wt<br>15</div>
																							<div class="calendar-day" data-date="2026-09-16" onclick="showday('2026-09-16')">Śr<br>16</div>
																							<div class="calendar-day" data-date="2026-09-17" onclick="showday('2026-09-17')">Cz<br>17</div>
																							<div class="calendar-day" data-date="2026-09-18" onclick="showday('2026-09-18')">Pt<br>18</div>
																							<div class="calendar-day" data-date="2026-09-19" onclick="showday('2026-09-19')">So<br>19</div>
																							<div class="calendar-day" data-date="2026-09-20" onclick="showday('2026-09-20')">Nd<br>20</div>
																							<div class="calendar-day" data-date="2026-09-21" onclick="showday('2026-09-21')">Pn<br>21</div>
																							<div class="calendar-day" data-date="2026-09-22" onclick="showday('2026-09-22')">Wt<br>22</div>
																							<div class="calendar-day" data-date="2026-09-23" onclick="showday('2026-09-23')">Śr<br>23</div>
																							<div class="calendar-day" data-date="2026-09-24" onclick="showday('2026-09-24')">Cz<br>24</div>
																							<div class="calendar-day" data-date="2026-09-25" onclick="showday('2026-09-25')">Pt<br>25</div>
																							<div class="calendar-day" data-date="2026-09-26" onclick="showday('2026-09-26')">So<br>26</div>
																							<div class="calendar-day" data-date="2026-09-27" onclick="showday('2026-09-27')">Nd<br>27</div>
																							<div class="calendar-day" data-date="2026-09-28" onclick="showday('2026-09-28')">Pn<br>28</div>
																							<div class="calendar-day" data-date="2026-09-29" onclick="showday('2026-09-29')">Wt<br>29</div>
																							<div class="calendar-day" data-date="2026-09-30" onclick="showday('2026-09-30')">Śr<br>30</div>
																																					<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
															<div class="calendar-day-placeholder"></div>
													</div>
					</div>

					<div style="text-align: right;" class="hidden-xs hidden-sm col-md-1 col-lg-1 fa fa-arrow-right"  style="color: #B3B3B3;" ></div>
				</div>
						<div class="col-xs-12 headline-box showall" onclick="showall();">zobacz wszystko</div>
		</div>
		<h2 style="text-align: center; margin-top: 25px;">Wybierz kategorię</h2>
		<div class="category-btn">
			<div class="flip concert">
				<div class="front">
				</div>
				<div class="back" onClick="showcategory(1);">
					<h2>Koncert</h2>
				</div>
			</div>
			<div class="flip spectacle">
				<div class="front">
				</div>
				<div class="back" onClick="showcategory(4);">
					<h2>Spektakl</h2>
				</div>
			</div>
			<div class="flip cabaret">
				<div class="front">
				</div>
				<div class="back" onClick="showcategory(5);">
					<h2>Kabaret</h2>
				</div>
			</div>
			<div class="flip others">
				<div class="front">
				</div>
				<div class="back" onClick="showcategory(6);">
					<h2>Inne</h2>
				</div>
			</div>
			<div class="seperator-vertical"></div>
			<div style="display: flex;">
				<div class="flip games" style="display: none;">
					<div class="front">
					</div>
					<div class="back" onClick="showcategory(7);">
						<h2>Games room</h2>
					</div>
				</div>

				<div class="flip cinema">
					<div class="front">
					</div>
					<div class="back" onClick="showcategory(2);">
						<h2>Kino</h2>
					</div>
				</div>
				<div class="flip product">
					<div class="front">
					</div>
					<div class="back" onClick="window.location.href='/product';">
						<h2>Produkty</h2>
					</div>
				</div>
			</div>
		</div>



		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7 headline-events">
			<h1>
									Repertuar:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7" style="display: none;">
			<h1>
									Kino:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7" style="display: none;">
			<h1>
									Koncert:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7" style="display: none;">
			<h1>
									Spektakl:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7" style="display: none;">
			<h1>
									Kabaret:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-5 col-lg-7" style="display: none;">
			<h1>
									Pozostałe:
							</h1>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-12 col-lg-12 repertoire-once row no-repertoire">
			<div class="date-separator">Brak wydarzeń w wybranym dniu</div>
		</div>

		<div style="clear: both;" class=""></div>

					<div class="row date-row 2026-06-10"></div>						<div class="cat-2 event-item event-item-2026-06-10 repertoire-once row 2026-06-10 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 10 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/925/1779166800.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - NIESAMOWITA HISTORIA MUMBO JUMBO" href="/index.php/repertoire.html?id=4895">NIESAMOWITA HISTORIA MUMBO JUMBO </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Malutki słonik Mumbo Jumbo magicznie urósł do gigantycznych rozmiarów. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Teraz musi wyruszyć w niebezpieczną podróż, aby odnaleźć Babę Jagę, która ma moce pozwalające na przywrócenie jego poprzednich rozmiarów. 
To pełna przygód wyprawa, która może sprawić, że Mumbo Jumbo znów stanie się mały, ale w środku pozostanie wielki.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 4+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 82"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>82''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 10 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - NIESAMOWITA HISTORIA MUMBO JUMBO" href="/index.php/repertoire.html?id=4895"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-10 repertoire-once row 2026-06-10 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 10 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/912/1776923211.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MANDALORIAN I GROGU  (dubbing)" href="/index.php/repertoire.html?id=4842">MANDALORIAN I GROGU  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Lucasfilm przedstawia "Gwiezdne wojny: The Mandalorian and Grogu", nową produkcję z uniwersum Gwiezdnych wojen, która zadebiutuje w kinach 22-ego maja 2026 roku, a zobaczymy w niej kolejną niesamowitą przygodę Mandalorianina i Grogu.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Złowieszcze Imperium upadło, a imperialni watażkowie rozpierzchli się po Galaktyce. Kiełkująca Nowa Republika pragnie ochronić wszystko, o co walczyła Rebelia. Werbuje więc legendarnego łowcę nagród, Mandalorianina Din Djarina (Pedro Pascal) i jego młodego podopiecznego Grogu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL/−−−<br>
Czas trwania: 120"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>132''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 10 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MANDALORIAN I GROGU  (dubbing)" href="/index.php/repertoire.html?id=4842"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-10 repertoire-once row 2026-06-10 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 10 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4901">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 10 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4901"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-10 repertoire-once row 2026-06-10 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 10 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4915">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 10 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4915"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-11"></div>						<div class="cat-2 event-item event-item-2026-06-11 repertoire-once row 2026-06-11 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 11 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/912/1776923211.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MANDALORIAN I GROGU  (dubbing) - KINO DLA SENIORA" href="/index.php/repertoire.html?id=4887">MANDALORIAN I GROGU  (dubbing) - KINO DLA SENIORA</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Lucasfilm przedstawia "Gwiezdne wojny: The Mandalorian and Grogu", nową produkcję z uniwersum Gwiezdnych wojen, która zadebiutuje w kinach 22-ego maja 2026 roku, a zobaczymy w niej kolejną niesamowitą przygodę Mandalorianina i Grogu.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Złowieszcze Imperium upadło, a imperialni watażkowie rozpierzchli się po Galaktyce. Kiełkująca Nowa Republika pragnie ochronić wszystko, o co walczyła Rebelia. Werbuje więc legendarnego łowcę nagród, Mandalorianina Din Djarina (Pedro Pascal) i jego młodego podopiecznego Grogu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL/−−−<br>
Czas trwania: 120"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>132''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 11 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MANDALORIAN I GROGU  (dubbing) - KINO DLA SENIORA" href="/index.php/repertoire.html?id=4887"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-11 repertoire-once row 2026-06-11 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 11 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/912/1776923211.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MANDALORIAN I GROGU  (dubbing)" href="/index.php/repertoire.html?id=4843">MANDALORIAN I GROGU  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Lucasfilm przedstawia "Gwiezdne wojny: The Mandalorian and Grogu", nową produkcję z uniwersum Gwiezdnych wojen, która zadebiutuje w kinach 22-ego maja 2026 roku, a zobaczymy w niej kolejną niesamowitą przygodę Mandalorianina i Grogu.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Złowieszcze Imperium upadło, a imperialni watażkowie rozpierzchli się po Galaktyce. Kiełkująca Nowa Republika pragnie ochronić wszystko, o co walczyła Rebelia. Werbuje więc legendarnego łowcę nagród, Mandalorianina Din Djarina (Pedro Pascal) i jego młodego podopiecznego Grogu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL/−−−<br>
Czas trwania: 120"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>132''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 11 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MANDALORIAN I GROGU  (dubbing)" href="/index.php/repertoire.html?id=4843"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-11 repertoire-once row 2026-06-11 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 11 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4902">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 11 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4902"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-11 repertoire-once row 2026-06-11 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 11 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4916">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 11 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4916"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="98%" style="height: 4px;background: #5a8354;width: 98%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-12"></div>						<div class="cat-2 event-item event-item-2026-06-12 repertoire-once row 2026-06-12 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 12 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4924">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 12 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4924"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-12 repertoire-once row 2026-06-12 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 12 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4932">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 12 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4932"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-12 repertoire-once row 2026-06-12 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 12 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4903">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 12 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4903"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-1 event-item event-item-2026-06-12 repertoire-once row 2026-06-12 cat-1">
				<div class="event-date-separator" style="display: block;">piątek, 12 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1779861220.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Mr Lonely" href="/index.php/repertoire.html?id=5011">Mr Lonely </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;"><font color="#0a0a0a">

Mr Lonely zawita do</font><font color="#12731e" style="color: rgb(10, 10, 10);"> </font><font style="" color="#006600">Centralnej klubokawiarni</font><font color="#0a0a0a"> w ramach trasy koncertowej promującej debiutancki album, wydany w lutym po dwóch latach koncertowania w Polsce i Niemczech.

</font>
</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px;"><span style="font-weight: normal;">

Zapraszamy na czerwcowy wieczór w klimatach autorskiej muzyki inspirowanej Dave Matthews Band, Elvisem, Voo Voo, czy Raz Dwa Trzy.</span><br>
<br><span style="font-weight: normal;">
Różnorodny anglojęzyczny repertuar zespołu łączy blues-rockową energię z jazzowo-funkującym groovem, chwytliwą sekcją dętą i retro-charakterem.
</span><br><br><span style="font-weight: normal;">
Zapraszamy do świata Mr Lonely!
</span><br><br>Kiedy?</span><span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px;"><span style="font-weight: normal;"> piątek, 12 czerwca, start godz. 20:00</span><br>Gdzie?</span><span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;"> Centralna Klubokawiarnia (w budynku Pszczyńskiego Centrum Kultury), ul. Władysława Jagiełły 1, Pszczyna<br>
<br>
***<br>
Mr Lonely to krakowski zespół, który wyłonił się z chaosu bluesowych dźwięków gitar i jazzowych melodii instrumentów dętych, zanurzonych w rockowej energii. Roztańczeni, rozmarzeni, różnorodni. Proponują melancholijny nastrój z przymrużeniem oka, prowadząc słuchacza przez zawiły labirynt rytmów i harmonii. Debiutancki krążek ukazał się we współpracy z czołowym ogólnopolskim kwartalnikiem „Twój Blues”.
<br><br>
"Jeśli lubicie elegancką muzykę z oldschoolowym sznytem, świetnie zaśpiewaną i w dodatku z wszechobecnymi dęciakami, Mr Lonely jest dla was!" M. Mieszczak, Portal Winylowy
<br><br>
Skład zespołu:<br>
Jakub Kulawik – wokal, trąbka, gitara elektryczna<br>
Andrzej Sendur – wokal, gitara akustyczna<br>
Jerzy Kulawik – instrumenty dęte<br>
Krzysztof Mysona – gitara basowa, kontrabas<br>
Bartłomiej Dybel – perkusja

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 40 PLN (w dniu koncertu 50 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 12 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Mr Lonely" href="/index.php/repertoire.html?id=5011"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="74%" style="height: 4px;background: #c65c28;width: 74%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-12 repertoire-once row 2026-06-12 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 12 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4917">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 12 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4917"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="94%" style="height: 4px;background: #5a8354;width: 94%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-13"></div>						<div class="cat-2 event-item event-item-2026-06-13 repertoire-once row 2026-06-13 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 13 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4925">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 13 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4925"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-13 repertoire-once row 2026-06-13 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 13 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4926">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 13 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4926"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-13 repertoire-once row 2026-06-13 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 13 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4933">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 13 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4933"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-13 repertoire-once row 2026-06-13 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 13 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4904">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 13 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4904"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-13 repertoire-once row 2026-06-13 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 13 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4918">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 13 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4918"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="94%" style="height: 4px;background: #5a8354;width: 94%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-14"></div>						<div class="cat-2 event-item event-item-2026-06-14 repertoire-once row 2026-06-14 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 14 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4927">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 14 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4927"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-14 repertoire-once row 2026-06-14 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 14 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4928">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 14 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4928"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-14 repertoire-once row 2026-06-14 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 14 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4934">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 14 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4934"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-14 repertoire-once row 2026-06-14 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 14 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4905">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 14 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4905"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="98%" style="height: 4px;background: #5a8354;width: 98%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-14 repertoire-once row 2026-06-14 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 14 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4919">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 14 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4919"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-15"></div>						<div class="cat-2 event-item event-item-2026-06-15 repertoire-once row 2026-06-15 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 15 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4929">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 15 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4929"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-15 repertoire-once row 2026-06-15 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 15 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4935">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 15 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4935"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-15 repertoire-once row 2026-06-15 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 15 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4906">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 15 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4906"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-15 repertoire-once row 2026-06-15 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 15 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4920">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 15 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4920"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-16"></div>						<div class="cat-2 event-item event-item-2026-06-16 repertoire-once row 2026-06-16 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 16 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4930">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 16 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4930"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-16 repertoire-once row 2026-06-16 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 16 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4936">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 16 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4936"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-1 event-item event-item-2026-06-16 repertoire-once row 2026-06-16 cat-1">
				<div class="event-date-separator" style="display: block;">wtorek, 16 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1779863594.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Gala uczniów Szkoły Muzycznej YAMAHA" href="/index.php/repertoire.html?id=5013">Gala uczniów Szkoły Muzycznej YAMAHA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Serdecznie zapraszamy na Galę uczniów Szkoły muzycznej YAMAHA podsumowującą rok szkolny 2025/2026.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

To nasza szkolna tradycja. Każdego roku, w czerwcu, spotykamy się wszyscy aby wspólnie cieszyć się z efektów edukacji muzycznej w naszej szkole wszystkich naszych uczniów. Na koncercie wystąpią uczniowie Pszczyńskiej placówki Yamaha Szkoła Muzyczna.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Na koncercie zaprezentują się następujące programy nauczania:<br>
<br>
• Programy wczesnodziecięce (dzieci od 4 roku życia)<br>
  - Junior Music Course<br>
<br>
• Programy instrumentalne<br>
  - gitara klasyczna<br>
  - gitara elektryczna<br>
  - keyboard<br>
<br>
• Szkoła Muzyki Rozrywkowej YAMAHA<br>
  - klasa wokalna <br>
  - pianoforte

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Na naszym koncercie usłyszycie Państwo solistów, zespoły instrumentalne oraz zespoły wokalno - instrumentalne prezentujące różnorodny repertuar od muzyki klasycznej do rozrywkowej. Po części artystycznej nastąpi uroczyste wręczenie dyplomów.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

BILETY: <br>
Bilet normalny:  30 PLN<br>
Dzieci do lat 7: bilet ulgowy w cenie 1 zł <br>
Dzieci od 7 do 16 lat: bilet ulgowy w cenie 20 zł<br>
Wstęp wolny dla wszystkich uczniów Szkoły YAMAHA – uczniowie wchodzą na karty zaproszenia VIP.

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 16 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Gala uczniów Szkoły Muzycznej YAMAHA" href="/index.php/repertoire.html?id=5013"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="59%" style="height: 4px;background: #5a8354;width: 59%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-16 repertoire-once row 2026-06-16 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 16 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4907">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 16 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4907"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-16 repertoire-once row 2026-06-16 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 16 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4921">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 16 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4921"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-17"></div>						<div class="cat-2 event-item event-item-2026-06-17 repertoire-once row 2026-06-17 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 17 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/928/1779172221.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4931">WILLOW I TAJEMNICZY LAS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Willow odziedziczyła nie tylko cały majątek po swej babci, ale także tajemną księgę magii. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Okazuje się, że jest potomkinią całej llinii czarownic. Przy pomocy Księgi przywołuje czarownika Grimoo, który ma jej pomóc obudzić służące jej moce ognia. 
<br>
Wszystko byłoby piękne gdyby nie to, że jej ojciec planuje sprzedać las, z którego wywodzi się cała magia, którą Willow posiada. Nie ma już wiele czasu aby obudzić w sobie pełną moc magii i wraz z trzema innymi czarownicami uratować istnienie pradawnego, czarodziejskiego lasu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 6+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 17 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WILLOW I TAJEMNICZY LAS" href="/index.php/repertoire.html?id=4931"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-17 repertoire-once row 2026-06-17 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 17 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4937">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 17 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4937"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-17 repertoire-once row 2026-06-17 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 17 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4908">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 17 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4908"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-17 repertoire-once row 2026-06-17 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 17 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4922">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 17 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4922"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-18"></div>						<div class="cat-2 event-item event-item-2026-06-18 repertoire-once row 2026-06-18 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 18 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/930/1779172864.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - POSŁANI  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4939">POSŁANI  – KINO DLA SENIORA</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Poruszająca opowieść o Bogu działającym tu i teraz — w życiu zwykłych ludzi, w ich decyzjach, kryzysach i przełomach. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Film opowiada o modlitwie jako realnej sile oraz o wspólnocie, która potrafi podtrzymać człowieka, gdy sam już nie daje rady. 
<br>
Osią opowieści jest niecodzienna droga Michała Ulewińskiego, który przemierza niemal 650 kilometrów przez Polskę, niosąc 15-kilogramowy krzyż. Trasa — od Zalewu Wiślanego po Giewont, dalej przez Gniezno aż do Sokółki — układa się w symboliczny znak krzyża na mapie kraju. To nie tylko wysiłek fizyczny. To intensywna, osobista modlitwa, duchowa walka i proces głębokiej wewnętrznej przemiany.
<br>
Wokół tej historii splatają się losy innych bohaterów — ludzi, których życie zostało nagle zatrzymane przez kryzys, spotkanie lub decyzję, po której nic nie było już takie samo. Ich świadectwa budują opowieść o tęsknocie za sensem, o bólu, który potrafi stać się początkiem nowego życia, i o nadziei, która nie gaśnie nigdy. Film pokazuje również żywą, dynamiczną rzeczywistość współczesnych wspólnot  takich jak Męski Różaniec, Wojownicy Maryi czy grupy modlitewne gromadzące tysiące osób w Polsce i poza jej granicami. Dotyka także napięć i wyzwań, przed którymi stoi dziś Kościół, oraz rosnącej roli świeckich w jego odnowie.
<br>
Reżyser Dariusz Walusiak, twórca m.in. „Ulmowie. Błogosławionej rodziny” i „Teraz i w godzinę śmierci” prowadzi widza przez historię, która stawia pytania o wiarę i zmianę — pokazując, że mogą one wydarzać się w życiu każdego człowieka.
<br>
Siła „Posłanych” tkwi w ich autentyczności. To opowieść, w której łatwo odnaleźć własne pytania i pęknięcia. Film przypomina, że przemiana nie jest zamkniętym rozdziałem historii. Może wydarzyć się dziś, tu i teraz, w życiu każdego człowieka.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: PL<br>
Napisy: −−−<br>
Czas trwania: 85"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>85''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 18 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - POSŁANI  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4939"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="98%" style="height: 4px;background: #5a8354;width: 98%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-18 repertoire-once row 2026-06-18 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 18 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/929/1779172546.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4938">K-POPOWE ŁOWCZYNIE DEMONÓW </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Rumi, Mira i Zoey to gwiazdy K-popu...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Rumi, Mira i Zoey to gwiazdy K-popu, które - gdy akurat nie koncertują na wypełnionych po brzegi stadionach - potrafią przedzierzgnąć się w nieposkromione łowczynie demonów, zawsze gotowe ochronić fanów przed wszechobecnymi zagrożeniami nie z tego świata. Razem muszą stawić czoła największemu jak dotąd wrogowi - pełnemu nieodpartego uroku konkurencyjnemu boysbandowi, którego członkami tak naprawdę są demony.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 10+<br>
Język: EN<br>
Napisy: PL+KARAOKE<br>
Czas trwania: 99"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>99''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 18 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - K-POPOWE ŁOWCZYNIE DEMONÓW" href="/index.php/repertoire.html?id=4938"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-18 repertoire-once row 2026-06-18 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 18 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/926/1779171424.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4909">WŁADCY WSZECHŚWIATA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dziesięcioletni Adam rozbija się na Ziemi, planecie swojej matki.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W trakcie lądowania traci Miecz Mocy Grayskull, należący do jego przodków. Po 15 latach rozłąki Miecz Mocy prowadzi księcia Adama (Nicholas Galitzine) z powrotem do Eternii, gdzie odkrywa, że jego ojczyzna legła w gruzach pod okrutnymi rządami Skeletora (Jared Leto). Aby ocalić swoją rodzinę i świat, Adam musi połączyć siły z najbliższymi sojusznikami, Teelą (Camila Mendes) i Duncanem/Man-At-Arms (Idris Elba), oraz zaakceptować swoje prawdziwe przeznaczenie jako He-Mana — najpotężniejszego człowieka we wszechświecie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 135"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>135''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 18 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - WŁADCY WSZECHŚWIATA" href="/index.php/repertoire.html?id=4909"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-1 event-item event-item-2026-06-18 repertoire-once row 2026-06-18 cat-1">
				<div class="event-date-separator" style="display: block;">czwartek, 18 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1772189503.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - 5. edycja SIĘGAJ GWIAZD W PSZCZYNIE − PIWNICA POD BARANAMI" href="/index.php/repertoire.html?id=4535">5. edycja SIĘGAJ GWIAZD W PSZCZYNIE − PIWNICA POD BARANAMI </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Koncert "Czy pamiętasz..." − piosenki Ewy DEMARCZYK i Marka GRECHUTY w interpretacjach artystów Piwnicy pod Baranami.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Wydarzenie Sięgaj Gwiazd w Pszczynie, ma na celu poznanie gwiazd, ludzi sceny nieco bliżej. Młodzież z naszego regionu zaprezentuje własne aranżacje piosenek związanych z Gwiazdą Wieczoru, a wszystko to, poprzeplatane będzie rozmową.<br>
<br>
Na zakończenie spotkania artystycznego wystąpi PIWNICA POD BARANAMI. 

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Podczas wydarzenia będzie można wesprzeć zbiórkę charytatywną na rzecz Dawidka Nycza, który choruje na postępującą chorobę genetyczną − rdzeniowy zanik mięśni typu 2.<br>
<br>
Goście specjalni :<br>
PIWNICA POD BARANAMI (koncert)<br>
Bogdan Micek − Dyrektor Artystyczny Piwnicy pod Barami (rozmowa)<br>
Konrad Mastyło - muzyk, kompozytor, pianista współpracujący przez lata m.in. z Piwnicą pod Baranami czy Markiem Grechutą w zespole Anawa. (rozmowa)<br>
<br>
oraz<br>
<br>
wokaliści ze Studia Piosenki VoicePless pod kierunkiem Barbary Brody-Malon<br>
chór dziecięcy "Piano-Forte" działający przy ZSP w Radostowicach i POPP w Pszczynie pod kierunkiem Katarzyny Machnik<br>
uczniowie Ogniska Muzycznego w Pszczynie<br>
uczniowie "Lekcje Bartek" − Bartłomiej Dzida<br>
uczniowie Studia Muzycznego Szymon Tworzy − Szymon Pałyz<br>
<br>
Zespół muzyczny "Sięgaj Gwiazd w Pszczynie": Bartłomiej Dzida, Sebastian Malon, Szymon Pałyz.<br>
<br>
Prowadzenie koncertu: Barbara Broda-Malon i Michał Czernek<br>
<br>
Partnerzy wydarzenia: salon Diva w Pszczynie, BULEK Studio

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 90 PLN (ulgowe 80 PLN dla osób uczących się, emerytów, rencistów oraz dla osób posiadających Kartę Mieszkańca Moja Pszczyna) do nabycia w Pszczyńskim Centrum Kultury oraz na stronie: bilety.pckul.pl

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 18 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - 5. edycja SIĘGAJ GWIAZD W PSZCZYNIE − PIWNICA POD BARANAMI" href="/index.php/repertoire.html?id=4535"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="12%" style="height: 4px;background: #3d879c;width: 12%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-18 repertoire-once row 2026-06-18 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 18 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/927/1779171724.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4923">STRASZNY FILM </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Dwadzieścia sześć lat po tym...
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Dwadzieścia sześć lat po tym, jak udało im się uciec przed podejrzanie znajomym zamaskowanym zabójcą, stała ekipa z serii STRASZNYCH FILMÓW znów znalazła się na celowniku mordercy i żadna seria horrorów nie jest bezpieczna.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 95"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>95''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 18 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:35				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - STRASZNY FILM" href="/index.php/repertoire.html?id=4923"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-19"></div>						<div class="cat-2 event-item event-item-2026-06-19 repertoire-once row 2026-06-19 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 19 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4941">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 19 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4941"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-19 repertoire-once row 2026-06-19 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 19 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4948">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 19 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4948"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-1 event-item event-item-2026-06-19 repertoire-once row 2026-06-19 cat-1">
				<div class="event-date-separator" style="display: block;">piątek, 19 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/266/1774861148.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Magiczny Koncert – Bajki Świata" href="/index.php/repertoire.html?id=4714">Magiczny Koncert – Bajki Świata </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Koncert z okazji Dnia Dziecka

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Ogólnopolski bestseller koncertowy – niezwykła muzyczna podróż w świat ulubionych bajek. Bohaterowie zejdą z ekranu, by podzielić się najpiękniejszymi i najbardziej znanymi utworami.<br>
<br>
Widowisko pełne kolorowych strojów i świateł, baniek mydlanych, śniegu, śmieszne dialogi, wspólna zabawa, namalują uśmiechy na twarzach młodych widzów.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Na samym początku przypomnimy to, co najważniejsze – „już się nie martw, aż do końca swych dni, naucz się tych dwóch, radosnych słów: Hakuna matata”. Następnie wypłyniemy na wielkie wody razem z Vaianą, gdzie wiatr poniesie nas ku przygodzie. Na scenie pojawi się umięśniony Maui, z którym wspólnie zaśpiewamy. Rrazem z Królem Julianem powyginamy śmiało ciało, chwycimy lianę z Tarzanem i wylądujemy na gorącej sawannie, gdzie króluje legendarny Król Lew. Na zaczarowanym dywanie polecimy z Aladynem by wspólnie z Elzą wyczarować prawdziwy śnieg. Na zakończenie tej niezwykłej podróży spotkamy sprytną króliczkę i bystrego lisa, którzy przypomną nam coś bardzo ważnego – by „nie bać się chcieć”.<br>
<br>
Dla kogo jest koncert?<br>
Dla dzieci, młodzieży, rodziców i wszystkich, którzy nadal mają w sercu dziecko. To nie tylko świetna zabawa, ale też wartościowa lekcja – o przyjaźni, dobru, odwadze i marzeniach.<br>
<br>
Co zyskują dzieci?<br>
• Rozbudzenie wyobraźni i ciekawości świata,<br>
• Wrażliwość na muzykę i sztukę,<br>
• Kontakt z wartościami poprzez bajkowe historie,<br>
• Możliwość aktywnego uczestnictwa w kulturze i rozwój talentów muzycznych.
<br>
<br>
Barwne kostiumy, bańki mydlane, muzyka i czarodziejska atmosfera – to niezapomniany koncert dla całej rodziny! Przybij piątkę z bohaterem! Zaśpiewaj swoją ulubioną piosenkę razem z postacią z bajki! A po koncercie – bezpłatne zdjęcia z bohaterami, na pamiątkę tego bajkowego spotkania.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Ważne informacje:<br>
• Jest to koncert, dlatego momentami może być głośniej.<br>
• Używamy kolorowych świateł, które czasami wędrują na publiczność.<br>
• Dzieci do lat 2 wchodzą bezpłatnie (miejsce na kolanach opiekuna).<br>
<br>
Po koncercie jest możliwość bezpłatnego sfotografowania się z wybranymi postaciami - na pamiątkę tego bajkowego spotkania.<br>
<br>
Czas trwania: około 65 minut fantastycznej zabawy bez przerwy!

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 60 / 50 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>65''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 19 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Magiczny Koncert – Bajki Świata" href="/index.php/repertoire.html?id=4714"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="28%" style="height: 4px;background: #5a8354;width: 28%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-19 repertoire-once row 2026-06-19 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 19 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4957">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 19 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4957"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-19 repertoire-once row 2026-06-19 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 19 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4970">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 19 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4970"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="93%" style="height: 4px;background: #5a8354;width: 93%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-20"></div>						<div class="cat-2 event-item event-item-2026-06-20 repertoire-once row 2026-06-20 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 20 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4949">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 20 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4949"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-20 repertoire-once row 2026-06-20 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 20 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4942">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 20 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4942"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-20 repertoire-once row 2026-06-20 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 20 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4950">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 20 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4950"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-20 repertoire-once row 2026-06-20 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 20 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4958">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 20 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4958"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-20 repertoire-once row 2026-06-20 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 20 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4971">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 20 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4971"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-21"></div>						<div class="cat-2 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4951">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4951"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4943">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4943"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4952">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4952"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-4 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-4">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1780918732.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Addamsowie w pętli czasu" href="/index.php/repertoire.html?id=5017">Addamsowie w pętli czasu </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Co by się stało, gdyby Wednesday Addams dostała szansę zmienić własny los?

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Addamsowie w pętli czasu” to autorski musical inspirowany światem Rodziny Addamsów. To pełna czarnego humoru, emocji i muzycznych przebojów opowieść o rodzinie, przyjaźni, dojrzewaniu i poszukiwaniu własnej tożsamości. Postacie Rodziny Addamsów od lat fascynują odbiorców swoją miłością do wszystkiego, co mroczne, dziwne i niepokojące. 

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

W dniu swoich szesnastych urodzin Wednesday zostaje uwikłana w tajemniczą klątwę, która rozrywa granice czasu. Na scenie spotykają się dziecięce, nastoletnie i dorosłe wersje bohaterów, a przeszłość, teraźniejszość i przyszłość zaczynają istnieć jednocześnie.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czy można zmienić własne przeznaczenie? Czy da się być Addamsem i jednocześnie być „normalnym”? I co stanie się ze światem, gdy ktoś spróbuje odrzucić część samego siebie?<br>
<br>
Przy dźwiękach największych światowych przebojów widzowie zostaną zabrani w podróż przez rodzinne sekrety, pętle czasu, alternatywne rzeczywistości i wybory, które mogą zmienić wszystko.<br>
<br>
To historia o tym, że czasem największą odwagą jest zaakceptowanie tego, kim naprawdę jesteśmy.<br>
<br>
Występują: Klaudia Jaromin, Zuzanna Grudzień, Hanna Białoń, Anna Sobańska, Natalia Stacha, Monika Retek, Natalia Nowak, Aleksandra Strządała, Ewelina Strządała, Barbara Strządała, Martyna Jabłeka, Hanna Jabłeka, Anna Rosmus, Karolina Figiel, Karolina Szczotka, Kamila Foltyn, Nikola Hajduga, Julia Hajduga, Julia Manowska<br>
<br>
Choreografia: Małgorzata Tomczyk<br>
<br>
Przygotowanie wokalne: Dorota Duda<br>
<br>
Scenariusz i reżyseria: Jakub Pieczka<br>
<br>
<strong>Przed spektaklem odbędzie się pokaz premierowy grupy młodzieżowej w "Dziadach" autorstwa i w reżyserii Filipa Kordiaczyńskiego</strong>

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 10 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Addamsowie w pętli czasu" href="/index.php/repertoire.html?id=5017"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="84%" style="height: 4px;background: #5a8354;width: 84%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-1 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-1">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/700/1781008602.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Relaksacyjny koncert kamertonowy" href="/index.php/repertoire.html?id=5020">Relaksacyjny koncert kamertonowy  </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Odzyskaj równowagę: Relaksacyjny koncert kamertonowy

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czy czujesz, że Twój organizm działa na „podwyższonych obrotach”, a chroniczne napięcie stało się Twoim cichym towarzyszem? Twój układ nerwowy potrzebuje profesjonalnego wsparcia, aby wyjść ze stanu ciągłego czuwania.
<br><br>
Zapraszam Cię na Relaksacyjny Koncert Kamertonowy – sesję, w której dźwięk staje się narzędziem głębokiej regeneracji Twojego ciała i umysłu.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

<strong>Dlaczego kamertony to wsparcie dla Twojego układu nerwowego?</strong>
<br><br>
W dzisiejszym świecie Twój układ nerwowy jest nieustannie przeciążony nadmiarem bodźców. Dźwiękoterapia kamertonami działa na poziomie biologicznym, wykraczając poza zwykły relaks:
<br><br>
    <strong>Wyciszenie układu współczulnego</strong>: Precyzyjne częstotliwości kamertonów pomagają przełączyć układ nerwowy z trybu „walki lub ucieczki” (odpowiedzialnego za stres) na tryb przywspółczulny, który jest niezbędny do regeneracji tkanek i narządów.
<br><br>
    <strong>Wprowadzenie w stany regeneracyjne</strong>: Wibracja dźwięku wspiera mózg w przechodzeniu z fal beta (aktywność, stres) do fal <strong>alfa i theta</strong>. To stany, w których ciało naturalnie rozpoczyna procesy samonaprawy i głębokiego odpoczynku.
<br><br>
    <strong>Redukcja chemicznych skutków stresu</strong>: Dzięki głębokiej relaksacji poziom kortyzolu – hormonu stresu – spada, co pozwala Twojemu organizmowi odetchnąć i odzyskać metaboliczną równowagę.
<br><br>
<strong>Co zyskasz dzięki temu spotkaniu?</strong>
<br><br>
    <strong>Ukojenie „rozchwianego” układu nerwowego</strong>: Poczujesz realną ulgę w napięciach mięśniowych i gonitwie myśli, co przyniesie Ci stan błogiego spokoju.
<br><br>
    <strong>Głęboka regeneracja</strong>: Odpoczynek podczas koncertu przekłada się na lepszą jakość snu i wyższy poziom energii w kolejnych dniach.
<br><br>
    <strong>Oczyszczenie z „przebodźcowania”</strong>: Dźwięk kamertonów działa jak „reset” dla Twojego umysłu, przywracając klarowność myślenia i wewnętrzną stabilność.
<br><br>
    <strong>Wzmocnienie odporności</strong>: Wprowadzając ciało w stan harmonii, wspierasz swoje naturalne mechanizmy obronne.
<br><br>
<strong>Dla kogo jest to wydarzenie?</strong>
<br><br>
Jeśli czujesz, że Twoje baterie są na wyczerpaniu, ten koncert jest dla Ciebie. To idealne rozwiązanie, jeśli:
<br><br>
    Zmagasz się z trudnościami w wyciszeniu się i zasypianiu.
<br><br>
    Potrzebujesz skutecznej metody na redukcję stresu w Twoim zabieganym grafiku.
<br><br>
    Szukasz bezpiecznej, holistycznej drogi do odzyskania wewnętrznego spokoju.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

<strong>Informacje organizacyjne</strong>
<br><br>
    <strong>Kiedy</strong>: 21 czerwca (niedziela), godz. 18:00 – idealny czas na ładowanie baterii przed nowym tygodniem.
<br><br>
    <strong>Miejsce</strong>: pckul, ul. Jagiełły 1 (I piętro, sala taneczna).
<br><br>
    <strong>Inwestycja w Twój spokój</strong>: 70 zł 
<br><br>
    <strong>Kontakt</strong>: 505 183 735
<br><br>
<strong>Co zabrać?</strong> Wygodny strój, matę, ulubiony koc oraz poduszkę – zadbaj o swój komfort.
<br><br>
Liczba miejsc jest ograniczona, aby zapewnić każdemu uczestnikowi intymną i kojącą przestrzeń. Nie odkładaj swojego dobrostanu na później – podaruj sobie ten czas. Zaproś bliską osobę, która również potrzebuje chwili wytchnienia od codzienności.
<br><br>
<strong>Do zobaczenia w wibracji spokoju!</strong>
<br><br>
Dźwięcznie zaprasza,<br>
Ewa Stroińska

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 70 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Relaksacyjny koncert kamertonowy" href="/index.php/repertoire.html?id=5020"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #c65c28;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4959">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4959"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-21 repertoire-once row 2026-06-21 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 21 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4972">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 21 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4972"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-22"></div>						<div class="cat-2 event-item event-item-2026-06-22 repertoire-once row 2026-06-22 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 22 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4944">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 22 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4944"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-22 repertoire-once row 2026-06-22 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 22 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4953">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 22 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4953"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-22 repertoire-once row 2026-06-22 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 22 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4960">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 22 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4960"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-22 repertoire-once row 2026-06-22 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 22 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4973">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 22 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4973"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-23"></div>						<div class="cat-2 event-item event-item-2026-06-23 repertoire-once row 2026-06-23 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 23 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4945">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 23 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4945"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-23 repertoire-once row 2026-06-23 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 23 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4954">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 23 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4954"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-4 event-item event-item-2026-06-23 repertoire-once row 2026-06-23 cat-4">
				<div class="event-date-separator" style="display: block;">wtorek, 23 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1780921634.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Przyjaźń. Po co nam to?" href="/index.php/repertoire.html?id=5018">Przyjaźń. Po co nam to? </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

W świecie, który nieustannie przyspiesza, coraz częściej zadajemy sobie pytanie: czy potrafimy jeszcze naprawdę być ze sobą?

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

„Przyjaźń. Po co nam to?” to intymny monodram Niny Tabak inspirowany głośnym esejem opublikowanym na łamach magazynu „Pismo”. To teatralna opowieść o relacjach, które budują naszą tożsamość, o ludziach, którzy pojawiają się w naszym życiu na chwilę i o tych, którzy zostają na zawsze.
<br><br>
Bohaterka spektaklu prowadzi widzów przez własne wspomnienia, doświadczenia i pytania dotyczące bliskości, samotności oraz potrzeby bycia zauważonym. W świecie pełnym pośpiechu, mediów społecznościowych i nieustannej komunikacji próbuje odnaleźć odpowiedź na jedno, pozornie proste pytanie: czym właściwie jest przyjaźń?
<br><br>
To poruszająca, szczera i pełna emocji teatralna podróż, która zachęca do zatrzymania się na chwilę i spojrzenia na własne relacje z nowej perspektywy.
<br><br>
Bo być może najważniejsze więzi w naszym życiu nie zawsze są tymi, o których mówi się najgłośniej.
<br><br>
Występuje: Nina Tabak
<br><br>
Reżyseria: Jakub Pieczka
<br><br>
Lokalizacja: sala sesyjna Starostwa Powiatowego w Pszczynie

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: BEZPŁATNE WEJŚCIÓWKI

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 23 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Przyjaźń. Po co nam to?" href="/index.php/repertoire.html?id=5018"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #c65c28;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-23 repertoire-once row 2026-06-23 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 23 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4961">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 23 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4961"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-23 repertoire-once row 2026-06-23 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 23 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4974">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 23 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4974"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="98%" style="height: 4px;background: #5a8354;width: 98%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-24"></div>						<div class="cat-2 event-item event-item-2026-06-24 repertoire-once row 2026-06-24 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 24 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4946">MICKEY I NICKY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 24 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY" href="/index.php/repertoire.html?id=4946"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-24 repertoire-once row 2026-06-24 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 24 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4955">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 24 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4955"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-4 event-item event-item-2026-06-24 repertoire-once row 2026-06-24 cat-4">
				<div class="event-date-separator" style="display: block;">środa, 24 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1780922341.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Nore + Panny z Wilka" href="/index.php/repertoire.html?id=5019">Nore + Panny z Wilka </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Zapraszamy na dwa ostatnie spektakle w ramach tegorocznych Owacji na bis!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

<strong>"Nore", 24.06.2026, 17:00, pckul</strong>
<br><br>
Nore pojawia się niespodziewanie. Jest zawsze blisko. Słucha. Doradza. Wspiera. Przynajmniej na początku.
<br><br>
„Nore” to poruszający spektakl inspirowany autobiograficzną powieścią graficzną Elisabeth Karin Pavón Rymre-Rythén „Moja przyjaciółka Nore”. To historia młodej dziewczyny, która próbuje odnaleźć swoje miejsce w świecie pełnym oczekiwań, ocen i nieustannych porównań. Kiedy w jej życiu pojawia się Nore, wszystko wydaje się prostsze. Wystarczy słuchać jej rad. Wystarczy być lepszą. Silniejszą. Doskonalszą. Z czasem jednak przyjaźń zamienia się w pułapkę.
<br><br>
Spektakl opowiada o samotności, potrzebie akceptacji i walce z głosem, który potrafi brzmieć jak najlepszy przyjaciel, choć prowadzi ku samozniszczeniu. To historia o odzyskiwaniu siebie, o odwadze proszenia o pomoc i o nadziei, która pojawia się nawet wtedy, gdy wydaje się, że jest już za późno.
<br><br>
„Nore” to teatralna opowieść dla młodzieży i dorosłych. Szczera, poruszająca i niezwykle aktualna. 
<br><br>
Bo czasem najtrudniejsza walka toczy się nie z innymi, lecz z głosem, który słyszymy we własnej głowie.
<br><br>
Występują: Melania Karpiel, Martyna Mikołajec, Iga Kuczowicz, Agata Trzcionka, Zuzanna Grudzień
<br><br>
Muzyka: Łukasz Brudek
<br><br>
Reżyseria: Jakub Pieczka
<br><br>
Choreografia: Małgorzata Tomczyk
<br><br>
__________________________________________

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

<strong>"Panny z Wilka", 24.06.2026, 17:45, pckul</strong>
<br><br>
Przez lata były wspomnieniem. Były młodością. Były tęsknotą. Były historią opowiadaną przez kogoś innego. A gdyby tym razem mogły opowiedzieć ją same?
<br><br>
„Panny z Wilka” to teatralna reinterpretacja opowiadania Jarosława Iwaszkiewicza, w której centrum opowieści znajdują się kobiety. Bohaterki znane z literackiego pierwowzoru odzyskują własny głos i własną perspektywę. Spotykają się po latach, przywołują dawne wydarzenia, wspomnienia, marzenia i rozczarowania. Rozmawiają o miłości, przemijaniu, wyborach oraz o życiu, które wydarzyło się pomiędzy spotkaniami.
<br><br>
Wiktor jest obecny nieustannie – w pamięci, w słowach, w emocjach – ale nigdy nie pojawia się na scenie. To nie jest jego historia.
<br><br>
To opowieść o kobietach, które przez lata były oglądane, oceniane i wspominane przez innych. O kobietach, które postanawiają opowiedzieć własną wersję wydarzeń. Kim były naprawdę? I czy pamięć o przeszłości należy do tego, kto odszedł, czy do tych, którzy zostali?
<br><br>
„Panny z Wilka” to spektakl o czasie, który nie zatrzymuje się dla nikogo, o sile kobiecych doświadczeń i o prawie do własnej narracji.
<br><br>
Występują: Barbara Sojka-Suchoński, Wiktoria Urbaniec, Natalia Kaczmarek, Julia Widera, Zofia Przemyk, Iga Kuczowicz
<br><br>
Reżyseria: Jakub Pieczka
<br><br>
Scenografia: Michalina Urbaniec

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 10 PLN (z tym samym biletem wejdziesz na oba spektakle)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 24 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Nore + Panny z Wilka" href="/index.php/repertoire.html?id=5019"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="98%" style="height: 4px;background: #5a8354;width: 98%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-24 repertoire-once row 2026-06-24 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 24 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4962">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 24 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4962"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-24 repertoire-once row 2026-06-24 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 24 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4975">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 24 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4975"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-25"></div>						<div class="cat-2 event-item event-item-2026-06-25 repertoire-once row 2026-06-25 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 25 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/931/1779173197.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MICKEY I NICKY  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4947">MICKEY I NICKY  – KINO DLA SENIORA</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Najlepszy amerykański film lat 70., jakiego jeszcze nie widzieliście. Tylko w kinach!
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jedno z arcydzieł amerykańskiego kina lat 70., które w Polsce dopiero czeka na odkrycie. Mikey i Nicky został zrodzony w twórczych bólach: zdjęcia trwały aż 110 dni, nakręconego materiału było niesłychanie dużo, a montaż zajął dwa lata. Gangsterska tragikomedia Elaine May o dwóch przyjaciołach i zawodowej zdradzie to kameralna nocna odyseja dziejąca się w zaśmieconej Filadelfii – „mieście braterskiej miłości”, po którym w tym samym roku biegał już Rocky Sylvestre’a Stallone’a.
<br>
May, matka chrzestna amerykańskiej komedii improwizowanej, była w tamtej dekadzie jedyną kobietą, która reżyserowała filmy dla hollywoodzkich studiów. Dzięki jej uporowi i niezmordowanej inwencji otrzymaliśmy w Mikey i Nicky niezapomniany występ dwóch mistrzów: Petera Falka, czyli słynnego porucznika Columbo, oraz Johna Cassavetesa, guru amerykańskiego kina niezależnego. Przygotujcie się na śmiech, który więźnie w gardle i rozmowy kumpli, które znajdują się o krok od krindżu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 106"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>106''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 25 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MICKEY I NICKY  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4947"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-25 repertoire-once row 2026-06-25 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 25 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/932/1779173554.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4956">DRZEWO MAGII </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Spektakularna i pełna ciepła ekranizacja na podstawie uwielbianej serii książek dla dzieci autorstwa Enit Blydon.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">W barwnej opowieści w role rodziców wcielają się laureat Złotego Globu, nominowany do Oscara Andrew Garfield („Sztuka pięknego życia”, „Spider Man: Bez drogi do domu”) oraz lauretka Złotego Globu Claire Foy („The Crown”). Towarzyszą im m.in. Nicola Coughlan – gwiazda serialu „Bridgertonowie”, hipnotyzująca w „Diunie” Rebecca Ferguson oraz niezapomniana z serialu „Reniferek” Jessica Gunning. Scenariusz jest dziełem Simon Farnaby’ego, współscenarzysty hitów "Paddington 2" i „Wonka". Polly i Tim wraz z trójką dzieci to współczesna rodzina, która staje przed koniecznością przeprowadzenia się na odległą angielską prowincję. Wkrótce po przyjeździe okazuje się, że najmłodsi muszą obejść się bez Wi-Fi i ukochanych elektronicznych gadżetów oraz odkryć uroki świata na świeżym powietrzu. Podczas eksploracji okolicznych lasów trafiają na niezwykłe drzewo, zamieszkane przez barwne, ekscentryczne istoty. Jeśli odważą się wspiąć na jego szczyt, czekają na nie fantastyczne krainy, pełne zapierających dech przygód. Dzięki magicznym doświadczeniom rodzina na nowo uczy się bycia razem i odkrywa, jak ważne jest wzajemne wsparcie i bliskość.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 8+<br>
Język: dubbing<br>
Napisy: −−−<br>
Czas trwania: 100"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>100''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 25 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - DRZEWO MAGII" href="/index.php/repertoire.html?id=4956"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-25 repertoire-once row 2026-06-25 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 25 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/930/1779172864.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - POSŁANI" href="/index.php/repertoire.html?id=4940">POSŁANI </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Poruszająca opowieść o Bogu działającym tu i teraz — w życiu zwykłych ludzi, w ich decyzjach, kryzysach i przełomach. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Film opowiada o modlitwie jako realnej sile oraz o wspólnocie, która potrafi podtrzymać człowieka, gdy sam już nie daje rady. 
<br>
Osią opowieści jest niecodzienna droga Michała Ulewińskiego, który przemierza niemal 650 kilometrów przez Polskę, niosąc 15-kilogramowy krzyż. Trasa — od Zalewu Wiślanego po Giewont, dalej przez Gniezno aż do Sokółki — układa się w symboliczny znak krzyża na mapie kraju. To nie tylko wysiłek fizyczny. To intensywna, osobista modlitwa, duchowa walka i proces głębokiej wewnętrznej przemiany.
<br>
Wokół tej historii splatają się losy innych bohaterów — ludzi, których życie zostało nagle zatrzymane przez kryzys, spotkanie lub decyzję, po której nic nie było już takie samo. Ich świadectwa budują opowieść o tęsknocie za sensem, o bólu, który potrafi stać się początkiem nowego życia, i o nadziei, która nie gaśnie nigdy. Film pokazuje również żywą, dynamiczną rzeczywistość współczesnych wspólnot  takich jak Męski Różaniec, Wojownicy Maryi czy grupy modlitewne gromadzące tysiące osób w Polsce i poza jej granicami. Dotyka także napięć i wyzwań, przed którymi stoi dziś Kościół, oraz rosnącej roli świeckich w jego odnowie.
<br>
Reżyser Dariusz Walusiak, twórca m.in. „Ulmowie. Błogosławionej rodziny” i „Teraz i w godzinę śmierci” prowadzi widza przez historię, która stawia pytania o wiarę i zmianę — pokazując, że mogą one wydarzać się w życiu każdego człowieka.
<br>
Siła „Posłanych” tkwi w ich autentyczności. To opowieść, w której łatwo odnaleźć własne pytania i pęknięcia. Film przypomina, że przemiana nie jest zamkniętym rozdziałem historii. Może wydarzyć się dziś, tu i teraz, w życiu każdego człowieka.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: PL<br>
Napisy: −−−<br>
Czas trwania: 85"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>85''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 25 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - POSŁANI" href="/index.php/repertoire.html?id=4940"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-25 repertoire-once row 2026-06-25 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 25 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/934/1779174545.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4976">BACKROOMS. BEZ WYJŚCIA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
W piwnicy salonu meblowego pojawia się przejście do przerażającego, równoległego świata.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Jeden z największych fenomenów internetu, obejrzana ponad 100 milionów razy seria filmów grozy, wkracza na wielki ekran.
<br>
Kane Parsons znany też jako Kane Pixels, który jako 16-latek stworzył serię internetowych filmów grozy, przedstawia jeden z najbardziej oryginalnych i przerażających horrorów ostatnich lat.
<br>
Gdzieś obok naszego świata istnieje inny, równoległy, nieskończony labirynt pozornie pustych korytarzy. Módl się, by tam nie trafić.
<br>
Nominowani do OSCARA Chiwetel Ejiofor („Zniewolony”) i Renate Reinsve („Wartość sentymentalna”) w filmie studia A24 („Hereditary. Dziedzictwo”), które znów przekracza gatunkowe ramy horroru.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 105"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>105''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 25 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - BACKROOMS. BEZ WYJŚCIA" href="/index.php/repertoire.html?id=4976"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-26"></div>						<div class="cat-2 event-item event-item-2026-06-26 repertoire-once row 2026-06-26 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 26 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4977">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 26 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4977"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-26 repertoire-once row 2026-06-26 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 26 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4984">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 26 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4984"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-26 repertoire-once row 2026-06-26 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 26 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4985">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 26 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4985"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-26 repertoire-once row 2026-06-26 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 26 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4963">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 26 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4963"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-27"></div>						<div class="cat-2 event-item event-item-2026-06-27 repertoire-once row 2026-06-27 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 27 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4986">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 27 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4986"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-27 repertoire-once row 2026-06-27 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 27 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4978">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 27 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4978"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-27 repertoire-once row 2026-06-27 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 27 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4987">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 27 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4987"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-27 repertoire-once row 2026-06-27 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 27 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4988">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 27 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4988"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-27 repertoire-once row 2026-06-27 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 27 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4964">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 27 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4964"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-28"></div>						<div class="cat-2 event-item event-item-2026-06-28 repertoire-once row 2026-06-28 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 28 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4989">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 10:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4989"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-28 repertoire-once row 2026-06-28 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 28 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4979">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4979"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-28 repertoire-once row 2026-06-28 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 28 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4990">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4990"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-28 repertoire-once row 2026-06-28 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 28 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4991">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4991"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-28 repertoire-once row 2026-06-28 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 28 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4965">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4965"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-29"></div>						<div class="cat-2 event-item event-item-2026-06-29 repertoire-once row 2026-06-29 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 29 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4980">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 29 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4980"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-29 repertoire-once row 2026-06-29 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 29 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4992">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 29 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4992"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-29 repertoire-once row 2026-06-29 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 29 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4993">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 29 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4993"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-29 repertoire-once row 2026-06-29 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 29 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4966">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 29 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4966"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-06-30"></div>						<div class="cat-2 event-item event-item-2026-06-30 repertoire-once row 2026-06-30 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 30 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4981">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 30 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4981"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-30 repertoire-once row 2026-06-30 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 30 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4994">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 30 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4994"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-30 repertoire-once row 2026-06-30 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 30 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4995">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 30 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4995"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-06-30 repertoire-once row 2026-06-30 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 30 czerwca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4967">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 30 czerwca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4967"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-01"></div>						<div class="cat-2 event-item event-item-2026-07-01 repertoire-once row 2026-07-01 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 1 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4982">CZYTAJĄC LOLITĘ W TEHERANIE </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 1 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 14:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE" href="/index.php/repertoire.html?id=4982"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-01 repertoire-once row 2026-07-01 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 1 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4996">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 1 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4996"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-01 repertoire-once row 2026-07-01 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 1 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4997">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 1 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4997"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-01 repertoire-once row 2026-07-01 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 1 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4968">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 1 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4968"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-02"></div>						<div class="cat-2 event-item event-item-2026-07-02 repertoire-once row 2026-07-02 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 2 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/935/1779174821.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4983">CZYTAJĄC LOLITĘ W TEHERANIE  – KINO DLA SENIORA</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Po rewolucji islamskiej w Iranie, która miała miejsce w 1979 roku, ulice Teheranu patrolowane są przez obrońców moralności, a fundamentaliści przejmują kontrolę nad uniwersytetami. 
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Kobiety muszą nosić hidżab, ich swobody są ograniczane, a lektura zachodniej literatury staje się aktem buntu. 
<br>
Taki stan rzeczy sprawia, że Azar Nafisi (Golshifteh Farahani), ambitna wykładowczyni literatury, rezygnuje z pracy na Uniwersytecie Teherańskim. Kobieta potajemnie zaczyna zapraszać do swojego domu grupę najbardziej zaangażowanych studentek. Razem czytają zakazane klasyki literatury zachodniej – „Lolitę” Vladimira Nabokova, „Wielkiego Gatsby’ego” F. Scotta Fitzgeralda, powieści Henry’ego Jamesa czy Jane Austen. Początkowo nieśmiałe młode kobiety stopniowo otwierają się – dzielą marzeniami, lękami, historiami miłosnymi oraz upokorzeniami związanymi z życiem w totalitarnym reżimie.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 15+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 108"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>108''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 2 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 13:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - CZYTAJĄC LOLITĘ W TEHERANIE  – KINO DLA SENIORA" href="/index.php/repertoire.html?id=4983"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-02 repertoire-once row 2026-07-02 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 2 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4998">SUPERGIRL  (dubbing)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 2 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (dubbing)" href="/index.php/repertoire.html?id=4998"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-02 repertoire-once row 2026-07-02 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 2 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4999">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 2 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=4999"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-2 event-item event-item-2026-07-02 repertoire-once row 2026-07-02 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 2 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/933/1779173860.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4969">ROBIN HOOD: KONIEC LEGENDY </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
Legenda była kłamstwem. Zapomnijcie o historii, którą znacie. Nadchodzi najmroczniejsza i najbardziej bezkompromisowa opowieść o legendarnym banicie.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">Robin Hood (Hugh Jackman), który do tej pory wiódł życie bandyty — naznaczone przemocą, krwią i cierpieniem — pogodził się z tym, że każda kolejna bitwa może być jego ostatnią. Jednak los pisze dla niego inny scenariusz. Ciężko ranny trafia pod opiekę tajemniczej kobiety (Jodie Comer), której córce grozi śmiertelne niebezpieczeństwo. Stając w jej obronie, przekonuje się, że walka o niewinne dziecko może być najważniejszą bitwą w jego życiu.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 16+<br>
Język: EN<br>
Napisy: PL<br>
Czas trwania: 123"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>123''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 2 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 20:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ROBIN HOOD: KONIEC LEGENDY" href="/index.php/repertoire.html?id=4969"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-03"></div>						<div class="cat-2 event-item event-item-2026-07-03 repertoire-once row 2026-07-03 cat-2">
				<div class="event-date-separator" style="display: block;">piątek, 3 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5000">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 3 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5000"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-04"></div>						<div class="cat-2 event-item event-item-2026-07-04 repertoire-once row 2026-07-04 cat-2">
				<div class="event-date-separator" style="display: block;">sobota, 4 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5001">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 4 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5001"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-05"></div>						<div class="cat-2 event-item event-item-2026-07-05 repertoire-once row 2026-07-05 cat-2">
				<div class="event-date-separator" style="display: block;">niedziela, 5 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5002">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 5 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5002"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-06"></div>						<div class="cat-2 event-item event-item-2026-07-06 repertoire-once row 2026-07-06 cat-2">
				<div class="event-date-separator" style="display: block;">poniedziałek, 6 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5003">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> poniedziałek, 6 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5003"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-07"></div>						<div class="cat-2 event-item event-item-2026-07-07 repertoire-once row 2026-07-07 cat-2">
				<div class="event-date-separator" style="display: block;">wtorek, 7 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5004">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> wtorek, 7 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5004"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-08"></div>						<div class="cat-2 event-item event-item-2026-07-08 repertoire-once row 2026-07-08 cat-2">
				<div class="event-date-separator" style="display: block;">środa, 8 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5005">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> środa, 8 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5005"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-07-09"></div>						<div class="cat-2 event-item event-item-2026-07-09 repertoire-once row 2026-07-09 cat-2">
				<div class="event-date-separator" style="display: block;">czwartek, 9 lipca 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/936/1779175249.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5006">SUPERGIRL  (napisy PL)</a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="text-size-adjust: 100%; vertical-align: baseline; color: black; line-height: 1.2; font-family: Calibri, sans-serif; text-align: left;">
<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">
UWAGA! UWAGA! Nadlatuje SUPERGIRL – i to w wersji, jakiej widzowie jeszcze nie widzieli.
</h3>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">To nie jest klasyczna, grzeczna bohaterka – to kobieta rakieta: nieidealna, zadziorna, emocjonalna i absolutnie nie do zatrzymania.
<br>
Nowa odsłona DC to kino o wyraźnym charakterze: dynamiczne, bezkompromisowe i „z pazurem”. Supergirl zabiera widzów w kosmiczną, widowiskową podróż, pełną akcji, nietuzinkowych bohaterów i wyrazistych przeciwników. Kiedy nieoczekiwany i bezwzględny przeciwnik atakuje niebezpiecznie blisko domu, Kara Zor-El, znana też jako Supergirl, niechętnie łączy siły z zaskakującym towarzyszem w pełnej przygód, międzygalaktycznej podróży w poszukiwaniu zemsty i sprawiedliwości.
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
    __________
</span>
<br>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">
KATEGORIA WIEKOWA: 12+<br>
Język: dubbing/EN<br>
Napisy: PL<br>
Czas trwania: 98"
</span>
</th>
<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kino-color.png" width="50" height="50">
</th>
</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>98''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 9 lipca 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:15				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SUPERGIRL  (napisy PL)" href="/index.php/repertoire.html?id=5006"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="100%" style="height: 4px;background: #5a8354;width: 100%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-09-26"></div>						<div class="cat-1 event-item event-item-2026-09-26 repertoire-once row 2026-09-26 cat-1">
				<div class="event-date-separator" style="display: block;">sobota, 26 września 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1781011842.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Śląsko-Góralska Biesiada z TVS" href="/index.php/repertoire.html?id=5021">Śląsko-Góralska Biesiada z TVS </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Już 26 września Telewizja TVS zabierze widzów w wyjątkową muzyczną podróż, łączącą dwa niezwykle barwne i pełne tradycji światy – Śląsk i kulturę góralską.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Na scenie Pszczyńskiego Centrum Kultury zacznie rządzić muzyka, która od pokoleń buduje tożsamość tych regionów, a wszystko to w nowoczesnej, energetycznej odsłonie.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Tego wieczoru publiczność porwą znani i lubiani artyści: Paweł Gołecki, Mateusz Szymczyk, Karolina Trela, Jula, Yejku oraz Beathris. Każdy z nich wniesie na scenę swój unikalny styl, prezentując zarówno tradycyjne brzmienia, jak i ich współczesne interpretacje.<br>
W repertuarze nie zabraknie zarówno śląskich szlagierów, jak i góralskich nut pełnych energii i emocji. To spotkanie kultur pokaże, jak wiele łączy te dwa światy  – od zamiłowania do muzyki, przez silne przywiązanie do tradycji, aż po niezwykłą gościnność i radość wspólnego świętowania.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czy artyści udowodnią, że Ślązak i Góral to naprawdę dwa bratanki? Wszystko wskazuje na to, że czeka nas wieczór pełen wzruszeń, tańca i niezapomnianych chwil. To wydarzenie, którego nie można przegapić – zarówno dla miłośników folkloru, jak i tych, którzy dopiero chcą odkryć jego wyjątkowy urok.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 110 / 90 PLN (ulgowe 90 / 80 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 26 września 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Śląsko-Góralska Biesiada z TVS" href="/index.php/repertoire.html?id=5021"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="93%" style="height: 4px;background: #5a8354;width: 93%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-01"></div>						<div class="cat-1 event-item event-item-2026-10-01 repertoire-once row 2026-10-01 cat-1">
				<div class="event-date-separator" style="display: block;">czwartek, 1 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1772690955.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Heban Hebanowski w Centralnej" href="/index.php/repertoire.html?id=4546">Heban Hebanowski w Centralnej </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;"><span style="color: rgb(10, 10, 10);">

Kolejny fantastyczny koncert w<font color="#12731e"> Centralnej klubokawiarni</font> przed nami!

</span>
</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Heban Hebanowski - artysta zakorzeniony w Krakowie i słyszany w Bieszczadach (m. in.wielokrotnie gościł na Festiwalu Chmielowisko). Występował też w Katowicach, Wrocławiu i Warszawie. Jego piosenki zabrzmią też w Pszczynie.<br>
Hebanowski od ponad dekady tworzy autorskie piosenki inspirowane codziennością i historią, naturą i miastem. Jego "kawałki heblowane", czyli "piosęki" cechują oryginalne teksty, zmienne nastroje łączące nostalgię i refleksję (nas troje) z humorem i energią (kocham cię), które dostarcząją osobistych przeżyć.<br>
Nagrał cztery albumy ze swoimi utworami: “trylogię heblowaną” (debiutanckie kawałki nieheblowane 2016 i albumy Szary człowiek 2017 oraz Przebłysk 2018. Ostatnia ukończona płyta to Ścieżki na wschód (cтежки на схід, eastbound 2023).<br>
Hebanowski hebluje nie tylko własne kawałki.<br>
Poza uprawianiem twórczości jest też pasjonatem muzyki tradycyjnej, doktorem (choć nie wypisuje recept) i badaczem folkloru miejskiego, a także współzałożycielem kapeli Wesoła Fala Band (muzyka XX-lecia międzywojennego) oraz Mołodców (męska grupa śpiewu tradycyjnego). <br>
Podczas koncertu w Pszczynie zaprezentuje wybrane piosenki (piosenki z sękiem).<br><br>
Zapraszamy!
</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 30 PLN (w dniu koncertu 35 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> czwartek, 1 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Heban Hebanowski w Centralnej" href="/index.php/repertoire.html?id=4546"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="96%" style="height: 4px;background: #c65c28;width: 96%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-02"></div>						<div class="cat-1 event-item event-item-2026-10-02 repertoire-once row 2026-10-02 cat-1">
				<div class="event-date-separator" style="display: block;">piątek, 2 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1780389023.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - SBB" href="/index.php/repertoire.html?id=5014">SBB </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Najlepszy polski rockowy zespół wystąpi w Pszczynie!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Legenda polskiego rocka wraca na scenę! SBB — zespół, który na zawsze odmienił brzmienie rodzimej muzyki — wystąpi w Pszczynie z wyjątkowym koncertem pełnym energii, wirtuozerii i ponadczasowych dźwięków. To spotkanie z muzyką, która od dekad inspiruje kolejne pokolenia słuchaczy.
<br><br>
Podczas koncertu usłyszysz kultowe kompozycje, improwizacje, charakterystyczne brzmienia i emocje, które od lat są znakiem rozpoznawczym SBB. To wydarzenie dla tych, którzy kochają muzyczne podróże, mistrzowskie wykonania i niepowtarzalny klimat koncertów na żywo.
<br><br>
Przyjdź i poczuj siłę muzyki, która nie starzeje się nigdy!

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 120 / 140 PLN (ulgowe –10 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 2 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - SBB" href="/index.php/repertoire.html?id=5014"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="92%" style="height: 4px;background: #5a8354;width: 92%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-11"></div>						<div class="cat-1 event-item event-item-2026-10-11 repertoire-once row 2026-10-11 cat-1">
				<div class="event-date-separator" style="display: block;">niedziela, 11 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1775113909.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - ŚWIATOWA GALA MUZYCZNA" href="/index.php/repertoire.html?id=4717">ŚWIATOWA GALA MUZYCZNA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Agencja Artystyczna MUZA zaprasza na ogólnopolską trasę koncertową, która już wkrótce zawita do największych miast Polski!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Przed Państwem niezwykła muzyczna podróż przez kontynenty, style i nastroje – koncert pełen emocji, barw i ponadczasowych melodii od arystokratycznego Wiednia, przez słoneczne Hawaje, Grecję, Hiszpanię, Włochy aż po roztańczone Rio!<br>

„Światowa Gala Muzyczna – od Wiednia do Rio de Janeiro” to prawdziwe święto muzyki – pełne pasji, nostalgii, zachwytu i radości życia.<br>

W programie zabrzmią majestatyczne walce wiedeńskie, gorące tanga argentyńskie, największe hity operetkowe, a także rozkołysane przeboje greckie, hiszpańskie i włoskie.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Wśród nich usłyszą Państwo m.in.:<br>
<br>
    Wirtuozowski Czardasz i poruszające Kiedy skrzypki grają<br>
    La Cumparsita<br>
    Grek Zorba<br>
    Y Viva España,<br>
    Ciao Bambino,<br>
    Mamma Maria<br>
    Bésame mucho<br>
    El porompompero<br>
    Lasciate mi cantare<br>
    Aria Torreadora<br>
    Egzotyczne tańce hula<br>
<br>
oraz karnawałowy finał w rytmach karnawałowej samby – Samba Brazylijska oraz Aquarela do Brasil.<br>
<br>
Wieczór ten wypełnią głosy znakomitych solistów:<br>
Monika Biederman-Pers – sopran<br>
Piotr Karzełek – baryton<br>
Jakub Oczkowski – tenor<br>
Marcin Korbut – bas<br>
z towarzyszeniem Orkiestry Duo Performance Band pod kierownictwem Mateusza Dudka<br>
<br>
Swoją energią scenę rozświetlą tancerki Bling Stars z Rio oraz mistrzowie tańców latynoamerykańskich i egzotycznych.<br>
<br>
Ten wieczór nie będzie zwykłym koncertem. To będzie opowieść – o uczuciach, rytmach, tradycji i pięknie.<br>
<br>
Muzyczne krajobrazy, taneczne emocje, światowa jakość i wyjątkowa energia – to wszystko czeka na Państwa podczas Światowe Gali Muzycznej!<br>
<br>
Scenariusz i reżyseria: Monika Biederman-Pers

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

„Światowa Gala Muzyczna – od Wiednia do Rio de Janeiro” – daj się porwać dźwiękom, które zostaną z Tobą na zawsze.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 120 / 160 / 180 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 11 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - ŚWIATOWA GALA MUZYCZNA" href="/index.php/repertoire.html?id=4717"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="54%" style="height: 4px;background: #5a8354;width: 54%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-16"></div>						<div class="cat-4 event-item event-item-2026-10-16 repertoire-once row 2026-10-16 cat-4">
				<div class="event-date-separator" style="display: block;">piątek, 16 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/861/1772538337.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Tajemnica 16. piętra" href="/index.php/repertoire.html?id=4541">Tajemnica 16. piętra </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Trzyma w napięciu jak najlepsza sensacja − bawi do łez jak najlepsza komedia.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Gdy w najwyższym biurowcu w centrum miasta ustaje gwar i kolejno gasną światła w biurach , na szesnastym piętrze zapala się światło.  To tu rozegra się  pełna zwrotów akcja spektaklu . Dwaj przyjaciele zostają przypadkowo wplątani w interesy dobrego znajomego.  Gdy orientują się , że sytuacja wymyka się spod kontroli na ucieczkę jest już za późno. Próbują odnaleźć się w nowej rzeczywistości , gdy wydaje się , że jest to możliwe pojawiają się dwaj gangsterzy  udający biznesmenów.  Zaczyna się jazda bez trzymanki. Bezwzględni bandyci postanowili nie przebierać w środkach i posunąć się do najgorszego...<br>
Co się wydarzyło? Czy ktoś zginie?  Czy był to czysty przypadek, czy może zaplanowana gra? Kto jeszcze stoi za intrygą?<br>
Mega dawka śmiechu, znakomite aktorstwo i dowcipne dialogi, gwarantowane.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czy masz odwagę odkryć, co dzieje się po zmroku na 16 piętrze w najwyższym biurowcu miasta? Gdy wszystko inne cichnie, tam zaczyna się prawdziwa akcja. Dwaj przyjaciele zostają wplątani w wydarzenia, w których stają twarzą w twarz z niebezpiecznymi gangsterami. Gdy sytuacja wymyka się spod kontroli, humor i napięcie sięgają zenitu!<br>
<br>
W „Tajemnicy 16 piętra” wszystko jest możliwe. Historia zaczyna się od niewinnego spotkania, które przeradza się w pełną adrenaliny przygodę. Dwaj bohaterowie, przypadkowo wplątani w kłopoty, muszą stawić czoła bezwzględnym przestępcom.<br>
<br>
To jednak dopiero początek. Każdy zwrot akcji prowadzi do kolejnych tajemnic, intryga goni intrygę, a kryminalna historia przeplata się z refleksjami o damsko-męskich relacjach. Czy masz odwagę zanurzyć się w wir szalonej przygody, w której każdy krok to niespodzianka?

</span>
</p>

<p>
<span style="color: rgb(255, 0, 0); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Aktorzy używają słów nieparlamentarnych.<br>
Spektakl dla widzów dorosłych.<br>
Uwaga! Używane są efekty stroboskopowe.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

OBSADA<br>
Michał Żurawski / Mariusz Ostrowski<br>
Tomasz Borkowski <br>
Piotr Liegenza/ Maciej Mikołajczyk<br>
Piotr Miazga<br>
Michał Sitarski/Dominik Bąk<br><br>

KOSTIUMY<br>
Katarzyna Adamczyk<br><br>

SCENARIUSZ i REŻYSERIA <br>
Jarosław Grzelka

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 140 PLN<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 16 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Tajemnica 16. piętra" href="/index.php/repertoire.html?id=4541"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="59%" style="height: 4px;background: #5a8354;width: 59%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-18"></div>						<div class="cat-1 event-item event-item-2026-10-18 repertoire-once row 2026-10-18 cat-1">
				<div class="event-date-separator" style="display: block;">niedziela, 18 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/813/1769150752.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Szlagierowa Uczta z Radiem Silesia" href="/index.php/repertoire.html?id=4223">Szlagierowa Uczta z Radiem Silesia </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

18 października 2026 r. (niedziela godz. 16:00) w Pszczyńskim Centrum Kultury koncert jakiego jeszcze nie było!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Dwie ikony szlagierowego rynku: Mariusz Kalaga oraz Toby z Monachium na jednej scenie!
<br><br>
Będzie pysznie, będzie smacznie, będzie niepowtarzalnie. Śpieszcie się z rezerwacją miejsc, bo coś nam mówi, że te znikną niczym ciepłe bułeczki. Bilety do kupienia w kasie pckulu oraz na stronie bilety.pckul.pl
<br><br>
Radio Silesia 96,2 fm – polecamy z całego serca!

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 110 / 95 PLN (ulgowe 95 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 18 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Szlagierowa Uczta z Radiem Silesia" href="/index.php/repertoire.html?id=4223"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="19%" style="height: 4px;background: #5a8354;width: 19%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-23"></div>						<div class="cat-1 event-item event-item-2026-10-23 repertoire-once row 2026-10-23 cat-1">
				<div class="event-date-separator" style="display: block;">piątek, 23 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1774601493.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Spięty powraca do Pszczyny!" href="/index.php/repertoire.html?id=4713">Spięty powraca do Pszczyny! </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Spięty powraca do Pszczyny z koncertem opartym na repertuarze z najnowszej płyty "Full H. D".

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czwarta płyta Spiętego to świat widziany oczami H.D., gdzie śmiech, ironia, pytania, obawy i lęki to jedynie część jego składowych. Na nowym albumie znajdzie się muzyka inspirowana tańcem ‒ zarówno jego tradycyjną formą jak bolero czy tango, ale również zupełnie współczesnym funkiem czy disco.<br>
Singiel „Blue”, promujący „Heartcore”‒ poprzedni album artysty, utrzymywał się nieprzerwanie na 1. miejscu listy przebojów Radia 357 przez 21 tygodni, a w radiowym TOP10 znajdował się przez przeszło 90 tygodni. Sam album „Heartcore” uzyskał nominację do nagrody Fryderyki 2023 w kategorii Muzyka Alternatywna.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Najnowszy singiel "Zapalanie Przedrostka" zadebiutował na 1 miejscu listy Radia 357 oraz na 2 miejscu Radia Nowy Świat.
Album osiągnął 1 miejsce na OLIS - w dniach 20.02.2026 - 26.02.2026 w trzech kategoriach: albumy, albumy fizyczne, winyle.<br>
HUBERT DOBACZEWSKI – wokalista, gitarzysta, jeden z najbardziej cenionych polskich tekściarzy, a przede wszystkim charyzmatyczny lider zespołu Spięty. Poza wspomnianym projektem Dobaczewski znany jest z takich zespołów jak Lao Che, Jazzombie czy Koli.<br>
W 2025 roku Hubert Dobaczewski został uhonorowany tytułem Ambasador Polszczyzny w Piśmie przyznawaną przez Radę Języka Polskiego.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Zespół koncertuje w składzie:<br>
Spięty − wokal, gitara<br>
Bartek Kapsa − perkusja, sampler<br>
Emil Wojtczak – bas<br>
Patryk Kraśniewski − klawisze

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 120 / 110 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 23 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Spięty powraca do Pszczyny!" href="/index.php/repertoire.html?id=4713"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="70%" style="height: 4px;background: #5a8354;width: 70%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-10-25"></div>						<div class="cat-4 event-item event-item-2026-10-25 repertoire-once row 2026-10-25 cat-4">
				<div class="event-date-separator" style="display: block;">niedziela, 25 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1775115543.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MIŁOŚĆ I POLITYKA" href="/index.php/repertoire.html?id=4718">MIŁOŚĆ I POLITYKA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Świat przedstawiony we francuskiej komedii, to duże pieniądze, wpływowi ludzie i wielkie tajemnice.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Komedia przenosi widza do miejsca, gdzie miłość, władza i pieniądze stanowią stawkę w nieustannej walce.<br>
<br>
Nie brakuje więc dynamicznej akcji, zaskakujących wydarzeń i dobrego humoru.<br>
<br>
Premier Bertrand Guéraud kocha ojczyznę i żonę, ale jeszcze bardziej rajcują go pieniądze. By dorobić się fortuny, prowadzi podejrzane interesy i notorycznie łamie prawo. Przez lata pławi się w luksusach i prowadzi życie jak z bajki. Nie spodziewa się, że w jednej sekundzie cały jego świat może lec w gruzach.<br>
<br>
A wszystko za sprawą teczki z kompromitującymi go materiałami, która trafiła w niepowołane ręce. Jak wysoką cenę będzie w stanie zapłacić, żeby uniknąć skandalu? Do czego posunie się, by prawda nie wyszła na jaw? Chyba nikt nie spodziewa się, jak zakończy się ta pełna zwrotów akcji historia.<br>
<br>
Znakomita komedia z plejadą polskich gwiazd takich jak Melania Grzesiewicz/Marysia Wieczorek, Anna Oberc, Łukasz Nowicki i Krystian Wieczorek/Jacek Król, Jacek Kopczyński/Mariusz Drężek odsłania prawdę o ludzkich namiętnościach i pokazuje, jak świat polityki wygląda od kuchni. A przy tym – bawi do łez.<br>
<br>
Skandalem byłoby jej nie zobaczyć!
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

komedia teatralna Pierre’a Sauvilla<br>
<br>
reżyseria: Anna Oberc<br>
<br>
przekład: Barbara Grzegorzewska<br>
<br>
scenografia: Witek Stefaniak

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 90 / 140 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 25 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 15:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MIŁOŚĆ I POLITYKA" href="/index.php/repertoire.html?id=4718"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="57%" style="height: 4px;background: #3d879c;width: 57%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

											<div class="cat-4 event-item event-item-2026-10-25 repertoire-once row 2026-10-25 cat-4">
				<div class="event-date-separator" style="display: block;">niedziela, 25 października 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1775115543.jpg" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - MIŁOŚĆ I POLITYKA" href="/index.php/repertoire.html?id=4719">MIŁOŚĆ I POLITYKA </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Świat przedstawiony we francuskiej komedii, to duże pieniądze, wpływowi ludzie i wielkie tajemnice.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Komedia przenosi widza do miejsca, gdzie miłość, władza i pieniądze stanowią stawkę w nieustannej walce.<br>
<br>
Nie brakuje więc dynamicznej akcji, zaskakujących wydarzeń i dobrego humoru.<br>
<br>
Premier Bertrand Guéraud kocha ojczyznę i żonę, ale jeszcze bardziej rajcują go pieniądze. By dorobić się fortuny, prowadzi podejrzane interesy i notorycznie łamie prawo. Przez lata pławi się w luksusach i prowadzi życie jak z bajki. Nie spodziewa się, że w jednej sekundzie cały jego świat może lec w gruzach.<br>
<br>
A wszystko za sprawą teczki z kompromitującymi go materiałami, która trafiła w niepowołane ręce. Jak wysoką cenę będzie w stanie zapłacić, żeby uniknąć skandalu? Do czego posunie się, by prawda nie wyszła na jaw? Chyba nikt nie spodziewa się, jak zakończy się ta pełna zwrotów akcji historia.<br>
<br>
Znakomita komedia z plejadą polskich gwiazd takich jak Melania Grzesiewicz/Marysia Wieczorek, Anna Oberc, Łukasz Nowicki i Krystian Wieczorek/Jacek Król, Jacek Kopczyński/Mariusz Drężek odsłania prawdę o ludzkich namiętnościach i pokazuje, jak świat polityki wygląda od kuchni. A przy tym – bawi do łez.<br>
<br>
Skandalem byłoby jej nie zobaczyć!
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

komedia teatralna Pierre’a Sauvilla<br>
<br>
reżyseria: Anna Oberc<br>
<br>
przekład: Barbara Grzegorzewska<br>
<br>
scenografia: Witek Stefaniak

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 90 / 140 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 25 października 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - MIŁOŚĆ I POLITYKA" href="/index.php/repertoire.html?id=4719"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="57%" style="height: 4px;background: #3d879c;width: 57%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-11-13"></div>						<div class="cat-5 event-item event-item-2026-11-13 repertoire-once row 2026-11-13 cat-5">
				<div class="event-date-separator" style="display: block;">piątek, 13 listopada 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1774597389.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Mariusz Kałamaga STAND UP" href="/index.php/repertoire.html?id=4712">Mariusz Kałamaga STAND UP </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Mamo! Papier się kończy!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Mariusz Kałamaga - na co dzień budzi ludzi w RMF FM i tam go tylko słychać. Wieczorami wychodzi ze swojej żeremi i budzi ludzi ze sceny i tam go widać.<br>
"Mamo! Papier się kończy!" to zestaw spostrzeżeń, przemyśleń, doświadczeń komika, które odpowiada myśli: Czyń Bobro miej dystans, ssij a będzie Ci ssane. Papier się kończy a wraz z nim pewna era. Nadchodzi sztuczna inteligencja i prawdziwa głupota. bo człowiek, by nie myśleć wymyślił robota. 
<br>
Duża dawka, energii i próba pozytywnego spojrzenia na ten żywot człowieka miejmy nadzieję jeszcze poczciwego.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 90 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kabaret-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 13 listopada 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:30				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Mariusz Kałamaga STAND UP" href="/index.php/repertoire.html?id=4712"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="9%" style="height: 4px;background: #c65c28;width: 9%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-11-27"></div>						<div class="cat-4 event-item event-item-2026-11-27 repertoire-once row 2026-11-27 cat-4">
				<div class="event-date-separator" style="display: block;">piątek, 27 listopada 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1774528843.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - (Nie) wszystko Ci oddam" href="/index.php/repertoire.html?id=4708">(Nie) wszystko Ci oddam </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Miłość, przyjaźń, radość życia – czegóż trzeba więcej, żeby czuć się dobrze z najbliższymi i żyć pełną piersią!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Przecież każdy potrzebuje jakiegoś organu do kochania. Sprawdźmy, czy to prawda, razem z Katarzyną, Dianą, Adamem i Tadeuszem – bohaterami współczesnej komedii napisanej przez popularnego austriackiego dramatopisarza Stefana Vögla.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Kiedy zaskoczona przez życie Katarzyna postanawia zdradzić swoim bliskim, co ją spotkało, to rusza lawina komediowych zdarzeń, zaskakujących reakcji i nieoczekiwanych deklaracji. Widz ma szansę sam się zastanowić, co by zrobił, słysząc miłosne wyznanie w stylu: „Masz w sobie coś, bez czego nie potrafię żyć. Podzielisz się ze mną?”.<br>
<br>
Zaskakujących pytań i zabawnych odpowiedzi będzie w spektaklu więcej, np.: Co można znaleźć na ostatnim piętrze najwyższego wieżowca w Warszawie? Jak długo należy zapiekać pieczeń z cynaderek? Czy współczesny człowiek jest gotów wspomóc najbliższą osobę drobną sumą pieniędzy, apartamentem w Barcelonie czy… No właśnie, czym jeszcze? To już muszą Państwo sami zobaczyć.<br>
<br>
Na kolejne przedstawienie teatralne w doborowej obsadzie, pełne humoru i mądrej rozrywki, zapraszają: Teatr Skene Warszawa i Dom Kultury w Rawiczu – producenci spektakli Cudowna terapia i Najsłodszy owoc.<br>
<br>
(Nie) wszystko ci oddam - czy to wyznanie miłości, zdrada, czy może ukryta intryga?

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

OBSADA<br>
<br>
Katarzyna: Joanna Moro / Joanna Osyda*<br>
Diana: Magdalena Walach / Ewelina Ruckgaber*<br>
Adam: Paweł Okraska / Miron Jagniewski*<br>
Tadeusz: Bilgun Ariunbaatar / Dariusz Taraszkiewicz*<br>
<br>
* wymiennie<br>
<br>
Polska prapremiera: 1 grudnia 2023 roku<br>
<br>
Producenci:<br>
Teatr Skene Warszawa Łukasz Niezgoda i Ewa Rączy<br>
Dom Kultury w Rawiczu<br>
Partner spektaklu: Prom Kultury Saska Kepa

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 140 / 100 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 27 listopada 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - (Nie) wszystko Ci oddam" href="/index.php/repertoire.html?id=4708"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="77%" style="height: 4px;background: #5a8354;width: 77%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-11-29"></div>						<div class="cat-1 event-item event-item-2026-11-29 repertoire-once row 2026-11-29 cat-1">
				<div class="event-date-separator" style="display: block;">niedziela, 29 listopada 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1779084379.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Ślązacy do wzięcia" href="/index.php/repertoire.html?id=4886">Ślązacy do wzięcia </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

„Ślązacy do wzięcia” – muzyczno-kabaretowy wieczór, którego nie zapomnisz!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Przygotuj się na prawdziwą ucztę dla duszy i serca! Najnowsza produkcja Teatru Muzycznego Castello to mieszanka tego, co najlepsze: wspaniałe głosy, taniec na najwyższym poziomie i humor, który rozbawi Cię do łez.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Na scenie pojawią się znakomici soliści – Tomasz Białek, Michał Marks i Tomasz Drogokupiec – którzy swoim talentem i charyzmą porwą publiczność w niezwykłą muzyczną podróż. Do tego widowiskowe choreografie w wykonaniu naszych tancerzy dodadzą blasku i energii, jakiej się nie spodziewasz.<br>
Całość poprowadzi w brawurowym stylu Sebastian Mierzwa, gwarantując nie tylko świetną zabawę, ale i ogromną dawkę śmiechu!

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

„Ślązacy do wzięcia” to wydarzenie, które łączy muzykę, taniec i kabaret w jedną wielką eksplozję pozytywnej energii.
To idealny sposób, by spędzić wieczór w wyjątkowej atmosferze i na chwilę zapomnieć o codzienności.<br>
<br>
Produkcja: Teatr Muzyczny Castello<br>
Management: Impresariat Artystyczny Kreatywna Pantera

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 130 / 110 PLN (ulgowe 120 / 100 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 29 listopada 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Ślązacy do wzięcia" href="/index.php/repertoire.html?id=4886"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="90%" style="height: 4px;background: #5a8354;width: 90%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-12-05"></div>						<div class="cat-1 event-item event-item-2026-12-05 repertoire-once row 2026-12-05 cat-1">
				<div class="event-date-separator" style="display: block;">sobota, 5 grudnia 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/890/1774518803.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Gwiazdorska Gala Wiedeńska – &quot;Wszystkie Drogi Prowadzą do Wiednia&quot;" href="/index.php/repertoire.html?id=4707">Gwiazdorska Gala Wiedeńska – "Wszystkie Drogi Prowadzą do Wiednia" </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Gwiazdorska Gala Wiedeńska – "Wszystkie Drogi Prowadzą do Wiednia" – pierwszy raz w Pszczynie z gościnnym udziałem Sohn Yeoi-Young !!!!


</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

W ten jeden, wyjątkowy wieczór zapraszamy Państwa na niepowtarzalne wydarzenie, które przeniesie słuchaczy w sam środek muzycznego raju – do złotego Wiednia, stolicy elegancji, walca i operetki.<br>
<br>
Gwiazdorska Gala Noworoczna to widowisko łączące tradycję z brawurą wykonania i niepowtarzalną atmosferą artystycznego święta.
Na scenie zabrzmią najpiękniejsze dzieła Straussów, Lehára i Kálmána - pulsujące rytmem walca, czarem melodii i wigorem noworocznej radości.<br>
<br>
Muzyczną stronę wieczoru z rozmachem i klasą poprowadzi Maestro Leszek Sojka, dyrygujący Polską Orkiestrą Królewską - zespołem złożonym z najwybitniejszych instrumentalistów, współpracujących na scenach z min.:takimi gwiazdami jak: Plácido Domingo, José Carreras, Andrea Bocelli, Aleksandra Kurzak czy Roberto Alagna.<br>
<br>
Ten wieczór to również święto wokalistyki. <br>
Wystąpią znakomici soliści - gwiazdy Opery Narodowej w Warszawie, Opery w Magdeburgu oraz K&amp;K Berliner Philharmoniker – artyści o wyjątkowym wyczuciu stylu i scenicznym magnetyzmie. Szczególne emocje wzbudzi z pewnością występ Anity Rywalskiej z – sopranistki obdarzonej nie tylko zjawiskowym głosem, lecz także charyzmą, która od lat zachwyca publiczność w Polsce i za granicą.<br>
<br>
Ornamentem tej olśniewającej gali będzie udział solistów Grand Royal Ballet, których taneczna gracja, precyzja i magnetyzm wprowadzą do wieczoru elementy wysublimowanego ruchu i bajkowej opowieści.<br>
<br>
To właśnie dzięki nim na scenie ożyją wiedeńskie salony, a walc znów zakręci światem. Przeżyj niezapomniany wieczór wśród dźwięków, świateł i emocji – pozwól, by muzyka poprowadziła Cię tam, gdzie serce Nowego Roku bije najmocniej: prosto do czarującego Wiednia.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 149 / 159 / 169 PLN (ulgowe od 139 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 5 grudnia 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="Kup bilet - Gwiazdorska Gala Wiedeńska – &quot;Wszystkie Drogi Prowadzą do Wiednia" href="/index.php/repertoire.html?id=4707"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="83%" style="height: 4px;background: #5a8354;width: 83%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2026-12-13"></div>						<div class="cat-1 event-item event-item-2026-12-13 repertoire-once row 2026-12-13 cat-1">
				<div class="event-date-separator" style="display: block;">niedziela, 13 grudnia 2026</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1769414573.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Back to the 90's Symfonicznie" href="/index.php/repertoire.html?id=4386">Back to the 90's Symfonicznie </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Back to the 90’s Symfonicznie - Największe Hity Lat 90' Symfonicznie w wykonaniu The Sound Generation Orchestra

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Zanurz się w dekadzie, która zdefiniowała całe pokolenie.
Wróć do czasów kaset magnetofonowych, pierwszych płyt CD, walkmanów i kultowych hitów, które rozbrzmiewały z ekranów MTV.
„Back to the 90’s Symfonicznie” to widowisko, jakiego jeszcze nie było — pełne emocji, energii i muzycznych zaskoczeń.

Na jednej scenie spotykają się dwa światy: rockowa energia, popowy błysk i majestat orkiestry. Za to niezwykłe brzmienie odpowiada The Sound Generation Orchestra – zespół, który łączy klasykę z nowoczesnością i pasją do ponadczasowych melodii.<br>
Towarzyszyć jej będą wyjątkowi soliści, którzy przeniosą publiczność prosto do złotej ery muzyki:<br>
Kamil Franczak – charyzmatyczny głos i sceniczna energia, która porywa od pierwszego dźwięku.<br>
Weronika Skalska – zjawiskowa wokalistka o barwie, która łączy emocje i elegancję.<br>
Gosia Janek – pełna pasji i autentyczności, wnosząca do każdego utworu świeżość i moc.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Podczas koncertu usłyszysz największe przeboje lat 90., które zyskały nowe życie w symfonicznych aranżacjach:<br><br>
„Vogue” – Madonna – elegancki, taneczny hołd dla królowej popu.<br>
„Everybody (Backstreet’s Back)” – Backstreet Boys – w potężnej wersji z chórem i sekcją dętą.<br>
„…Baby One More Time” – Britney Spears – z dramatyzmem i smyczkową wrażliwością.<br>
„Believe” – Cher – symfoniczna eksplozja wiary i emocji.<br>
„Wannabe” – Spice Girls – energetyczna zabawa z orkiestrą.<br>
„Smells Like Teen Spirit” – Nirvana – bunt, który nabiera monumentalnego wymiaru.<br>
„Niepokonani” – Perfect – finał, który poruszy serca i połączy pokolenia.<br>
<br>
To jednak nie wszystko!<br>
Na publiczność czekają muzyczne niespodzianki, których nie znajdziesz w żadnej zapowiedzi. Każdy moment tego koncertu to emocjonalna podróż, w której symfonia spotyka się z nostalgią, a znane melodie odkrywają przed nami nowe brzmienia.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Czeka Cię ponad 1,5 godziny (około 1:30) muzycznej energii, wspomnień i zupełnie nowych, niepowtarzalnych aranżacji – stworzonych specjalnie na tę okazję.<br><br>
„Music can change the world because it can change people.” – Bono
<br><br>
Przygotuj się na wieczór pełen emocji, wspólnego śpiewania i magii, która przeniesie Cię z powrotem do lat 90.<br>
To nie jest zwykły koncert – to muzyczne widowisko pełne niespodzianek, które na długo zostanie w pamięci.<br>
The Sound Generation Orchestra, wraz z Kamilem Franczakiem, Weroniką Skalską i Gosią Janek, zapraszają Cię na niezwykłe spotkanie z muzyką, która łączy pokolenia.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 160 / 150 / 140 PLN (ulgowe 150 / 140 / 130 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 13 grudnia 2026<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 18:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Back to the 90's Symfonicznie" href="/index.php/repertoire.html?id=4386"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="83%" style="height: 4px;background: #5a8354;width: 83%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2027-01-09"></div>						<div class="cat-1 event-item event-item-2027-01-09 repertoire-once row 2027-01-09 cat-1">
				<div class="event-date-separator" style="display: block;">sobota, 9 stycznia 2027</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1779439550.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Wiedeńska Noc: Gala Noworoczna z Grand Étoile Orchestra" href="/index.php/repertoire.html?id=5007">Wiedeńska Noc: Gala Noworoczna z Grand Étoile Orchestra </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

W niezwykłej oprawie muzyki, świateł i scenicznej elegancji zapraszamy Państwa na prawdziwie królewskie widowisko noworoczne – koncert galowy „Wiedeńska Noc”, który kontynuuje najlepsze tradycje wielkich europejskich gal noworocznych.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

To wydarzenie, które przeniesie publiczność do świata ponadczasowej klasyki, tańca i wokalnej wirtuozerii – świata pełnego emocji, wzruszeń i zachwytu. Międzynarodowy prestiż i muzyczna perfekcja.<br>
<br>
Na scenie wystąpi Grand Étoile Orchestra – wyjątkowy zespół symfoniczny złożony z wybitnych muzyków o międzynarodowym dorobku. Artyści tej formacji koncertują regularnie na prestiżowych estradach w Wiedniu, Pradze, Berlinie, Nowym Jorku i wielu innych miastach Europy oraz Ameryki Północnej. Orkiestra łączy elegancję klasycznego brzmienia z energią współczesnych interpretacji, prezentując repertuar od arii operowych po porywające walce Straussa. Jej muzycy związani są m.in. z renomowanymi projektami, takimi jak K&amp;K Philharmoniker, co gwarantuje najwyższy poziom wykonawczy i artystyczny.<br>
<br>
Wyjątkowy charakter wieczoru podkreślą soliści operowi związani z prestiżowymi instytucjami:<br>
– Opera Narodowa w Warszawie,<br>
– Teatr Wielki w Poznaniu,<br>
– Opera w Magdeburgu,<br>
– Opera Lwowska,<br>
a także uznane sceny w Austrii, Czechach, Niemczech i Stanach Zjednoczonych.<br>
<br>
Uzupełnieniem koncertu będą widowiskowe występy solistów baletowych Grand Étoile Ballet, którzy swoją scenicznością, techniką i emocjonalną ekspresją wzbogacą program o elementy pełne finezji i klasycznego uroku.
<br><br>
W programie koncertu:<br>
Najsłynniejsze walce i polki Johanna Straussa (ojca i syna)<br>
Operetkowe przeboje Lehára, Kálmána i Offenbacha<br>
Fragmenty dzieł Mozarta, Pucciniego, Bizeta<br>
Arie, duety i ansamble wokalne w wykonaniu międzynarodowych solistów<br>
Baletowe miniatury solowe i duety, inspirowane estetyką wiedeńskiego dworu<br>
Finał z udziałem wszystkich artystów – w blasku noworocznego entuzjazmu
<br><br>
Niezapomniany wieczór pełen klasy, emocji i blasku!<br>
<br>
„Wiedeńska Noc” to wydarzenie, które łączy wszystko, co najpiękniejsze w muzyce klasycznej: elegancję orkiestry, siłę ludzkiego głosu i magiczny świat tańca. To pełna splendoru gala, w której każdy detal – od repertuaru po wykonanie – składa się na artystyczne przeżycie najwyższej próby.
<br>
<br>
Czas trwania: około 2 godzin + przerwa

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: od 140 PLN

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/koncert-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>150''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> sobota, 9 stycznia 2027<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Wiedeńska Noc: Gala Noworoczna z Grand Étoile Orchestra" href="/index.php/repertoire.html?id=5007"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="88%" style="height: 4px;background: #5a8354;width: 88%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2027-02-28"></div>						<div class="cat-5 event-item event-item-2027-02-28 repertoire-once row 2027-02-28 cat-5">
				<div class="event-date-separator" style="display: block;">niedziela, 28 lutego 2027</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1773910274.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Kabaret Młodych Panów − Z żartami nie ma żartów" href="/index.php/repertoire.html?id=4564">Kabaret Młodych Panów − Z żartami nie ma żartów </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Kabaret Młodych Panów - Z żartami nie ma żartów - 20 lecie

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Już ponad dwie dekady Kabaretu Młodych Panów na scenie – pełne spektakularnych występów, szalonych podróży i zwrotów akcji, które mogłyby zawstydzić niejedną telenowelę. Gwarantujemy najlepsze żarty, najdziwniejsze przygody, a co najważniejsze – nieokiełznane poczucie humoru, które nie zna granic!

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

W programie „Z żartami nie ma żartów” widzowie spotkają się z bohaterami, którzy przez lata rozbawiali do łez. Ten program to mieszanka satyry, błyskotliwego humoru i kąśliwych obserwacji tego, co dzieje się na caluśkim świecie.<br>
<br>
Nowy program to nie tylko najlepsze z najlepszych, ale także coś zupełnie nowego! Dołączcie do nas, bo w kabarecie nie ma czasu na nudę! Przed nami kolejny rok humoru, śmiechu i… czegoś, czego nie da się opisać – trzeba to po prostu zobaczyć!

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Uwaga! Program „Z żartami nie ma żartów” może wywołać niekontrolowane ataki śmiechu, które prowadzą do bólu brzucha oraz chwilowego zapomnienia o codziennych troskach. W razie wątpliwości co do skuteczności zastosowanych żartów – skonsultuj się z najbliższymi. Jeżeli objawy rozweselenia utrzymują się dłużej niż 3 dni, niezwłocznie skonsultuj się z Kabaretem Młodych Panów.

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 160 / 140 PLN (ulgowe 140 PLN)

</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/kabaret-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 28 lutego 2027<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 16:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Kabaret Młodych Panów − Z żartami nie ma żartów" href="/index.php/repertoire.html?id=4564"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="5%" style="height: 4px;background: #c65c28;width: 5%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2027-04-11"></div>						<div class="cat-4 event-item event-item-2027-04-11 repertoire-once row 2027-04-11 cat-4">
				<div class="event-date-separator" style="display: block;">niedziela, 11 kwietnia 2027</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event//1770809673.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - Szczęściarze" href="/index.php/repertoire.html?id=4410">Szczęściarze </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

"Szczęściarze" autorstwa i w reżyserii Tadeusza Kuty, to hitowa polska komedia!

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Swoich bohaterów autor osadził we współczesnych realiach codzienności, na których jak grom z jasnego nieba spada "szczęście", które może nie tylko przynieść mnóstwo radości ale i przysporzyć wielu kłopotów. Dialogi i sceny wyjęte z życia każdego małżeństwa pozwalają przeglądać się Widzom jak w krzywym zwierciadle. Zapraszamy na przezabawną polską komedię! <br><br>
Scenariusz i reżyseria: Tadeusz Kuta<br> Scenografia: Joanna Biskup-Brykczyńska, Iwona Jamka<br> Muzyka: Wojciech Lisowicz
<br><br>
Obsada:<br>
Niuńcia - Teresa Bielińska<br>
Bolo - Mirosław Bieliński<br>
Prezes - Liwiusz Falak<br>
Biskup - Marek Kępiński / Mirosław Bieliński<br>
Wiceprezes - Karol Czajkowski / Grzegorz Karłowicz / Łukasz Oleś<br>
ks. Stefan - Łukasz Oleś / Jarosław Rabenda<br>
Pan Aniołek - Karol Czajkowski / Mateusz Drozdowski / Łukasz Oleś<br>
<br>
Producent spektaklu: Teatr TeTaTeT<br>
Organizator: Impresariat Artystyczny Kreatywna Pantera

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 130 / 110 PLN (ulgowe 120 / 100 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>0''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> niedziela, 11 kwietnia 2027<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 17:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - Szczęściarze" href="/index.php/repertoire.html?id=4410"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="91%" style="height: 4px;background: #5a8354;width: 91%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					<div class="row date-row 2027-05-21"></div>						<div class="cat-4 event-item event-item-2027-05-21 repertoire-once row 2027-05-21 cat-4">
				<div class="event-date-separator" style="display: block;">piątek, 21 maja 2027</div>
				<div class="co-xs-3 col-sm-2 col-md-2 col-lg-1 avaliable">
					<div class="repertoire-image-box-index">
						<div class="repertoire-image-bg">
							<img class="list-repertoire-image" src="/uploads/event/828/1776759044.png" />						</div>
					</div>
				</div>
				<div class="col-xs-9 col-sm-5 col-md-5 col-lg-6 title">
					<a title="Kup bilet - My Fair Lady" href="/index.php/repertoire.html?id=4387">My Fair Lady </a>					<br />
					<div style="height: 72px;overflow: hidden;" class="description">
						<table style="width: 100%;">
<tbody>
<tr>
<th style="box-sizing: border-box; margin: 0px 0px 0.5rem; padding: 0px 0px 10px; border: 0px; outline: 0px; text-size-adjust: 100%; vertical-align: baseline; background: rgb(255, 255, 255); line-height: 1.2; text-align: left;">

<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 16px; font-weight: bold;">

WYDARZENIE ORGANIZATORA ZEWNĘTRZNEGO

</span>

<h3 style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 20px; font-weight: bold;">

Jeden z najsłynniejszych musicali w historii teatru muzycznego, od lat obecny w repertuarach polskich scen już tej jesieni w pckulu.

</h3>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

Spektakl oparty jest na dramacie „Pigmalion” George’a Bernarda Shawa i opowiada historię prostej kwiaciarki Elizy Doolittle, która dzięki eksperymentowi fonetycznemu profesora Henry’ego Higginsa przechodzi niezwykłą metamorfozę. Celem naukowca jest nauczenie dziewczyny nienagannej wymowy i manier tak, by mogła uchodzić za damę z wyższych sfer. W trakcie tej przemiany rodzą się jednak pytania o godność, tożsamość i granice ingerencji w ludzkie życie.<br>
„My Fair Lady” zachwyca nie tylko inteligentnym, pełnym humoru librettem, lecz także niezapomnianą muzyką Fredericka Loewego i błyskotliwymi tekstami piosenek. Takie przeboje jak „Przetańczyć całą noc”, „Jeden mały szczęścia łut” czy „Tę ulicę znam” stały się jednymi z najbardziej popularnych piosenek musicalowych. Spektakl łączy elegancję, dowcip i wzruszenie, oferując widzom widowisko pełne barwnych kostiumów, tanecznych scen i ponadczasowego przesłania. Dlatego właśnie „My Fair Lady” niezmiennie cieszy się popularnością na scenach i przyciąga kolejne pokolenia widzów.

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

OBSADA:<br>
ELIZA DOOLITTLE – Anna Pieszka/ Laura Petzuch<br>
PROFESOR HIGGINS – Jarosław Tyran<br>
PUŁKOWNIK PICKERING – Sebastian Garbacz<br>
ALFRED DOOLITLE – Błażej Olma<br>
FRED EYNSFORD HILL – Tomasz Białek<br>
PANI PEARCE – Barbara Pakura-Brzoska<br>
PANI HIGGINS – Wanda Faruga<br>
DIVA OPEROWA – Justyna Dyla<br>
<br>
Tancerze i instrumentaliści Teatru Muzycznego Castello

</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">

REALIZATORZY: <br>
REŻYSERIA – Tomasz Białek<br>
CHOREOGRAFIA – Wiktoria Koryciak<br>
KOSIUMY – Tomasz Białek, Aneta Ratajczak<br>
SCENOGRAFIA – Tomasz Białek, Aneta Ratajczak<br>
REŻYSERIA ŚWIATŁA I DZWIĘKU – Tomasz Wolny<br>
ORGANIZACJA – Patryk Moch<br>
MANAGEMENT SPEKTAKLU – Impresariat Artystyczny KREATYWNA PANTERA

</span>
</p>

<p></p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: normal;">
__________
</span>
</p>

<p>
<span style="color: rgb(10, 10, 10); font-family: &quot;calibri light&quot;, Helvetica, Arial, Lucida, sans-serif; font-size: 18px; font-weight: bold;">

Bilety: 140 / 110 PLN (ulgowe 120 / 100 PLN)

<br>
</span>
</p>

</th>

<th style="vertical-align: top;">
<img src="https://pckul.pl/wp-content/uploads/2024/05/spektakl-color.png" width="50" height="50">
</th>

</tr>
</tbody>
</table>					</div>
					<button class="toggleButton">ROZWIŃ ▼</button>
									</div>
				<div class="col-xs-9 col-sm-3 col-md-3 col-lg-3 date">
					<i class="fas fa-history"></i> <i>120''</i> <br />
					<i class="fa fa-calendar" aria-hidden="true"></i> piątek, 21 maja 2027<br />
					<i class="fa fa-clock-o" aria-hidden="true"></i>godz. 19:00				</div>
				<div class="col-xs-12 col-sm-2 col-md-2 col-lg-2 link">
											<a class="button" title="&quot;Kup bilet - My Fair Lady" href="/index.php/repertoire.html?id=4387"><i class="fas fa-shopping-cart"></i> kup bilet</a>										<div class="avlbt">
												<div class="avlbt-box">
							Dostępność:
							<div class="avlbt-box-line">
								<div title="84%" style="height: 4px;background: #5a8354;width: 84%"></div>
							</div>
						</div>
					</div>
				</div>

			</div>

					</div>
	<div id="show-more-container" style="display:none; text-align: center;">
		<button id="show-more" class="button">Pokaż więcej</button>
	</div>
	<div class="hidden-xs hidden-sm hidden-md col-lg-1"></div>

	<script>
		function calendarViewportHeight() {
			if (window.visualViewport && window.visualViewport.height) {
				return window.visualViewport.height;
			}
			return window.innerHeight;
		}

		function isCalendarMobileLayout() {
			return window.matchMedia("(max-width: 767px)").matches;
		}

		function getCalendarMaxHeight() {
			var btn = document.querySelector(".btn-calendar");
			var vh = calendarViewportHeight();
			var margin = 16;
			var gap = isCalendarMobileLayout() ? 10 : 8;
			if (!btn) {
				return Math.min(560, Math.max(200, vh - margin * 2)) + "px";
			}
			var r = btn.getBoundingClientRect();
			var avail = vh - r.bottom - gap - margin;
			var cap = isCalendarMobileLayout() ? 520 : 560;
			return Math.min(cap, Math.max(200, avail)) + "px";
		}

		function positionCalendarPanel() {
			var btn = document.querySelector(".btn-calendar");
			var cal = document.getElementById("calendar");
			if (!btn || !cal) return;
			var r = btn.getBoundingClientRect();
			var gap = isCalendarMobileLayout() ? 10 : 8;
			var vw = window.innerWidth;
			var margin = 12;
			cal.style.top = (r.bottom + gap) + "px";
			if (isCalendarMobileLayout()) {
				cal.style.left = "50%";
				cal.style.transform = "translateX(-50%)";
				cal.style.width = "";
				return;
			}
			var centerX = r.left + r.width / 2;
			cal.style.transform = "translateX(-50%)";
			cal.style.width = "";
			var panelW = Math.min(620, vw);
			var half = panelW / 2;
			var clamped = Math.max(margin + half, Math.min(centerX, vw - margin - half));
			cal.style.left = clamped + "px";
		}

		function toggleCalendar() {
			var calendarElement = document.getElementById("calendar");
			if (calendarElement.style.display === "none" || calendarElement.style.display === "") {
				calendarElement.style.display = "flex";
				positionCalendarPanel();
				calendarElement.style.maxHeight = "0px";
				calendarElement.setAttribute("data-expanding", "true");
				setTimeout(function() {
					positionCalendarPanel();
					calendarElement.style.maxHeight = getCalendarMaxHeight();
					calendarElement.removeAttribute("data-expanding");
				}, 0);
			} else {
				calendarElement.style.maxHeight = "0px";
				calendarElement.setAttribute("data-expanding", "true");
				setTimeout(function() {
					calendarElement.style.display = "none";
					calendarElement.removeAttribute("data-expanding");
					calendarElement.style.width = "";
				}, 400);
			}
		}

		function repositionOpenCalendar() {
			var cal = document.getElementById("calendar");
			if (!cal || cal.style.display === "none" || cal.style.display === "") return;
			positionCalendarPanel();
			cal.style.maxHeight = getCalendarMaxHeight();
		}

		window.addEventListener("resize", repositionOpenCalendar);
		window.addEventListener("scroll", repositionOpenCalendar, true);
		if (window.visualViewport) {
			window.visualViewport.addEventListener("resize", repositionOpenCalendar);
		}

		document.addEventListener("click", function(event) {
			var calendar = document.getElementById("calendar");
			var isExpanding = calendar.getAttribute("data-expanding") === "true";
			var isClickInsideCalendar = calendar.contains(event.target);
			var isClickOnToggleBtn = event.target.closest(".btn-calendar") || event.target.closest(".calendar-toggle-button");
			var isClickInsideMonthDays = event.target.closest(".month-days");
			if ((!isClickInsideCalendar && !isClickOnToggleBtn && !isExpanding) || (isClickInsideCalendar && !isClickInsideMonthDays && !isExpanding)) {
				calendar.style.maxHeight = "0px";
				setTimeout(function() {
					calendar.style.display = "none";
				}, 300);
			}
		});

		$(document).ready(function() {
			$('.cat-2').prev('.date-row').hide();
			$('.cat-2').hide();
			$('.cat-7').prev('.date-row').hide();
			$('.cat-7').hide();
		});

		curday = 'all';

		let itemsPerPage = 20;
		let currentPage = 1;

		function showcategory(cat) {
			var backDivs = document.querySelectorAll('.flip .front');
			backDivs.forEach(function(div) {
				div.classList.remove('active-front-cat');
			});

			event.target.classList.add('active-front-cat');

			$('.repertoire-once').hide();
			// $('.date-separator').hide();
			$('.event-group-headline').hide();

			$('.headline-' + cat).slideDown();

			if (curday != "all") {
				$('.cat-' + cat + '.' + curday).slideDown();

				if (cat == 6) {
					$('.cat-1.' + curday).slideDown();
					$('.cat-2.' + curday).hide();
				}

			} else {
				if (cat == 2) {
					currentPage = 1;
					showMoreItems(cat);
					$('#show-more-container').show();
				} else {
					$('.cat-' + cat).slideDown();
					$('#show-more-container').hide();
				}
			}

			if ($('.cat-' + cat + ':visible').length > 0 || (cat == 6 && ($('.cat-2:visible').length > 0 || $('.cat-1:visible').length > 0))) {
				$('.no-repertoire').hide();
				$('.date-row').show();
			} else {
				$('.no-repertoire').slideDown();
			}

		}

		function showall() {
			curday = 'all';
			updateCalendarDaySelection(null);

			$('.repertoire-once').hide();
			$('.event-group-headline').hide();

			$('.cat-1, .cat-2, .cat-4, .cat-5, .cat-6, .cat-7').slideDown();

			$('#show-more-container').hide();
			$('.date-row').show();
			$('.no-repertoire').hide();
		}



		function showMoreItems(cat) {
			const items = document.querySelectorAll('.cat-' + cat);
			const totalItems = items.length;
			const itemsToShow = itemsPerPage * currentPage;

			for (let i = 0; i < itemsToShow && i < totalItems; i++) {
				items[i].style.display = 'block';
			}

			if (itemsToShow >= totalItems) {
				$('#show-more-container').hide();
			} else {
				$('#show-more-container').show();
			}
		}

		document.getElementById('show-more').addEventListener('click', function() {
			currentPage++;
			showMoreItems(2);
		});

		document.querySelectorAll('.flip .front').forEach(item => {
			item.addEventListener('click', event => {
				document.querySelectorAll('.flip .front').forEach(div => {
					div.classList.remove('active-front-cat');
				});

				event.currentTarget.classList.add('active-front-cat');
			});
		});


		document.querySelectorAll('.flip .back').forEach(item => {
			item.addEventListener('click', event => {
				document.querySelectorAll('.flip .front').forEach(div => {
					div.classList.remove('active-front-cat');
				});

				const parentFlip = event.currentTarget.closest('.flip');
				const frontDiv = parentFlip.querySelector('.front');
				frontDiv.classList.add('active-front-cat');
			});
		});
	</script>
	<script>
		document.addEventListener("DOMContentLoaded", function() {
			let elements = document.querySelectorAll('.event-date-separator');
			let seenTexts = new Set();
			let firstElementFound = false;

			elements.forEach(function(element) {
				let textContent = element.textContent.trim();
				let parentDiv = element.closest('.cat-6');

				if (seenTexts.has(textContent)) {
					element.style.display = 'none';
				} else {
					seenTexts.add(textContent);
				}

				if (!firstElementFound && parentDiv) {
					element.classList.add('first');
					firstElementFound = true;
				}
			});
		});
	</script>
	<script>
		function updateCalendarDaySelection(date) {
			document.querySelectorAll("#calendar .calendar-day").forEach(function(el) {
				var d = el.getAttribute("data-date");
				el.classList.toggle("is-selected", date && d === date);
			});
		}

		function showday(date) {
			curday = date;
			updateCalendarDaySelection(date);
			$('.repertoire-once').hide();
			$('.repertoire-once.' + date).show();

			if ($('.repertoire-once.' + date + ':visible').length > 0) {
				$('.no-repertoire').hide();
			} else {
				$('.no-repertoire').slideDown();
			}

			$('.date-row').hide();
			$('.repertoire-once.' + date).prev('.date-row').show();
		}
	</script>
	<script>
   document.querySelectorAll('.toggleButton').forEach(button => {
    button.addEventListener('click', function() {
        let description = this.previousElementSibling;
        
        if (description.classList.contains('expanded')) {
            description.style.height = description.scrollHeight + 'px';
            requestAnimationFrame(() => {
                description.style.height = '75px'; 
                this.textContent = 'ROZWIŃ ▼'; 
                setTimeout(() => {
                    description.classList.remove('expanded'); 
                }, 500); 
            });
        } else {
            description.classList.add('expanded'); 
            description.style.height = description.scrollHeight + 'px'; 
            this.textContent = 'ZWIŃ ▲';       
            setTimeout(() => {
                description.style.height = 'auto';
            }, 500); 
        }
    });
});

</script></div>
		  
		              <script type="text/javascript">
			  
			  var scrolluj = false;
			  var currency = '&#8364;';

              floatingBox = function() {
                var $floatingbox = $('#moving_cart');

                if ($('#body').length > 0) {
                  if ($(window).scrollTop() < 36) {
                    $('#hide_cart').css('visibility', "hidden");
					//$('.navigation2').slideUp();
					//$('.navigation').slideDown();
                  }
                  $(window).scroll(function() {
                        if ($(window).scrollTop() > 0) {
						  $('.navigation2').slideDown();
						  //$('.navigation').slideUp();
                        } else {
						  $('.navigation2').slideUp();
						  //$('.navigation').slideDown();
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
                  //var pos = $("#sb_cart").position();
                  //var pos = $("#sb_cart").position();
                  //var height = $("#sb_cart").height();
                  //sb_content.css('position', 'absolute')
                  //sb_content.css('left', pos.left + 300)
                  //sb_content.css('top', (pos.top + height + 100))
                  //sb_content.css('z-index', '1000')
                  //sb_content.slideDown(500)
                  //sb_content_label.text("Ukryj szczegóły koszyka");
                } else {
                  //sb_content.slideUp(500);
                  //sb_content_label.text("pokaż szczegóły koszyka");
                }
              }

              function animateCartError(error) {
                if ($("#sb_cart_content").css('display') != 'none') {
                  //$("#sb_cart_content").slideUp(500);
                  //$("#sb_cart_detail_label").text("pokaż szczegóły koszyka");
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
                  //sbCartDetailsAnimate();
                });
                cart.onLoaded(function() {
                  for (e in this._items) { // przelecenie po kazdym elemencie z koszyka
                    var item = this._items[e];
                    buildCartItem(item);
                  }
                  //console.log(this.count());
                  if (this.count() > 0) {
                    $("#cart_buy_button").show();
					$(".cart_buy_button_inactive").hide();
                    $("#sb_cart_detail_label").show();
                  } else {
                    $("#cart_buy_button").hide();
                    $("#sb_cart_detail_label").hide();
					$(".cart_buy_button_inactive").show();
                  }
                });
                cart.onLoading(function() {
                  $('.details_content').text('');
                });
                // obsluga dodawania elementu do koszyka
                cart.addOption('onCartAdd', function(Event, item) {

                  $('.details_content').append($('<div>dodawanie do koszyka</div>').attr('id', this.getUID()));
                  $("#cart_buy_button").show();
                  $("#sb_cart_detail_label").show();
				  $(".cart_buy_button_inactive").hide();
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
					
				    $('#cart-added-info').slideUp();
				    $('#cart-removed-info').slideDown();
				    $('#cart-removed-info').delay( 800 ).fadeOut('3000');					
					
                  } else {
                    $('#' + this.getUID()).remove();
                  }
                  $('#sb_cart .worth').text(this.getCart().getWorth());
                  //PS dane do externala
                  $('.cart_buy_button').attr('href', "/index.php/order/summary.html?external[quantity]=" + this.getCart().count() + "&external[worth]=" + this.getCart().getWorth());
                  if (this.getCart().count() <= 0) {
                    $("#cart_buy_button").hide();
                    $("#sb_cart_detail_label").hide();
					$(".cart_buy_button_inactive").show();
                    $('#sb_cart .count').text('0');
					$('.cart-small .count').text('0');
                    //$("#sb_cart_content").css('display', "none");
                    $("#sb_cart_detail_label").text("pokaż szczegóły koszyka");
                  } else {
                    $('#sb_cart .count').text(this.getCart().count());
					$('.cart-small .count').text(this.getCart().count());
                  }
				  
				  postIframeMessage(); //zmiana rozmiaru ramki
                });
                // obsluga dodawania elemntu do koszyka
                cart.addOption('onCartLoaded', function(Event) {
                  
				  $('#' + this.getUID()).removeClass('ui-state-disabled');
                  
				  buildCartItem(this);
				  
				  $('#cart-removed-info').slideUp();
				  $('#cart-added-info').slideDown();
				  $('#cart-added-info').delay( 800 ).fadeOut('3000');	  
				  
                });
                // obsluga ladowania (podczas dodawania lub usuwania)
                cart.addOption('onCartLoading', function(Event) {
				  $('#sb_cart .count').text('Aktualizacja koszyka');
                  $('#sb_cart .worth').text(' - ');
                  $('#' + this.getUID()).addClass('ui-state-disabled');
                });
                // obsluga ladowania (podczas dodawania lub usuwania)
                cart.addOption('onCartError', function(Event, code, error, method) {
                  animateCartError(error);
                  $('#sb_cart .count').text(this.getCart().count());
				  $('.cart-small .count').text(this.getCart().count());
                  $('#sb_cart .worth').text(this.getCart().getWorth());
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
                      if (repertoire_id == item.getRepertoireId() && parseInt(element.children('info').attr('avaiable_places')) <= 1) {
                        $('#path_' + element.attr('id')).trigger('item_select');
                      }
                    }
                  }
                } else if (this.TYPE == 'CartItemRepertoire') {

                }
                  $('#' + item.getUID()).text('').addClass('cart-product-once');
                  if (item.getDiscount()) {
                      var productName = 'Bilet zniżkowy';
                  } else {
                      var productName = item.getName().split(' - ');
                      var productName = productName[0];
                  }
				  
				  console.log(item.getDiscounts());
				  
				  //each(item.getDiscounts() as discount()) {
				  $.each(item.getDiscounts(), function(el, val){
					console.log(el);
				  });	  
				  
                  //widok wszystkie
                  var div_price = $('<div class="hidden-xs col-md-2 col-lg-2 pname">' + productName + '</div><div class="col-xs-2 col-md-1 col-lg-1 remove" onclick="if(' + item._place_id + '){deleteFromCart(' + item._repertoire_id + ', ' + item._product_id + ', ' + item._place_id + ')}else{cart.del(\'' + item._uid + '\')}"><i class="fas fa-times-circle"></i></div><div class="hidden-xs col-md-2 col-lg-2 pprice">' + item.getWorth() + '</div>');
                  $('#' + item.getUID()).append(div_price);
                  var div = null;
                  //widok sali numerowanej
                  if (item.TYPE == 'CartItemPlace') {
                      //console.log(item);
                      div = $('<div class="col-xs-4 col-md-3 col-lg-3 title">' + item.getInfo('event_name') + '<br/>' + item.getInfo('event_date') + '</div><div class="col-xs-4 col-md-3 col-lg-3 place">' + (item.getInfo('section').length > 0 ? 'sektor: <strong>' + item.getInfo('section') + '</strong>' : '') + ' ' + (item.getInfo('row').length > 0 ? 'rzad: <strong>' + item.getInfo('row') + '</strong> miejsce: <strong>' + item.getInfo('number') + '</strong>' : '<strong>miejsca nienumerowane</strong></div>') + '');

                  //+' rząd: <strong>'+item.getInfo('row')+'</strong> miejsce: <strong>'+item.getInfo('number')+'</strong></div>')

                  $('#' + item.getUID()).append(div);
                  //div = $('<div class="col-xs-6 col-md-2 col-lg-2 date">' + item.getInfo('event_date') + '</div>');
                  $('#' + item.getUID()).append(div);
                } else if (item.TYPE == 'CartItemRepertoire') {
                  //widok sali nienumerowanej
				  div = $('<div class="col-xs-4 col-md-3 col-lg-3 title">' + item.getInfo('event_name') + '<br/>' + item.getInfo('event_date') + '</div><div class="col-xs-6 col-md-3 col-lg-3 place">' + (item.getInfo('section').length > 0 && item.getInfo('section') != "miejsce nienumerowane" ? 'sektor: <strong>' + item.getInfo('section') + '</strong> - ' : '') + '<strong>miejsce nienumerowane</strong></div>');
                  $('#' + item.getUID()).append(div);
                  //div = $('<div class="col-xs-6 col-md-2 col-lg-2 date">' + item.getInfo('event_date') + '</div>');
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
				$('.cart-small .count').text(item.getCart().count());
                $('#sb_cart .worth').text(item.getCart().getWorth());
                //PS dane do externala
                $('.cart_buy_button').attr('href', "/index.php/order/summary.html?external[quantity]=" + item.getCart().count() + "&external[worth]=" + item.getCart().getWorth());
                $('#del_' + item.getUID()).click(function() {
                  item.del();
                });
				
				 postIframeMessage(); //zmiana rozmiaru ramki
              }
			  
			  
			  $(".tooltip3").hover(
			  function() {
				var sb_content = $("#tooltiptext");
				$('#tooltiptext').text($(this).attr('tooltip-title'));
				sb_content.css('position', 'absolute')
				$(this).on( "mousemove", function( event ) {
					//$( "#log" ).text( "pageX: " + event.pageX + ", pageY: " + event.pageY );
					sb_content.css('left', (event.pageX + 20))
					sb_content.css('top', (event.pageY - 50)) 
				});	
				//console.log(pos.left+' / '+sb_content.css('left'));
				sb_content.css('z-index', '1000')
				sb_content.slideDown(500)		
				//var pos = $(this).position();

			  }, function() {
				var sb_content = $("#tooltiptext");
				sb_content.slideUp(500).text('');
			  }
			  );			  
            </script>
                          <script type="text/javascript">
                $(function() {
                  cart.load();
                });
              </script>
					  
      </div>
	 	 		<div class="footer">
	<div class="row footer1"></div>
	<div class="row footer2">
		<div class="col-xs-12 col-sm-12 col-md-4 col-lg-4 params">
			 
			<a target="_blank" href="//systembiletowy.pl">System Sprzedaży Biletów visualTicket</a><br/><a target="_blank" href="//systembiletowy.pl">www.systembiletowy.pl</a>
			<br/><br/>
			<div style="float: left;">language: pl<br/>
			skin: pck2</div>
			<div style="float: left;margin: 10px 0px 0px 10px;">
				<a href="?culture=pl&id=" title="" class="selected">pl</a>
				<a href="?culture=en&id=" title="" >en</a>			
			</div>	
		</div>
		<div class="col-xs-12 col-sm-12 col-md-4 col-lg-4 params p2">	
			<i class="fas fa-barcode"></i>
		</div>
		<div class="col-xs-12 col-sm-12 col-md-4 col-lg-4 params p3">
			System owner: Pszczyńskie Centrum Kultury<br/><br/>
			
			Made with <i class="fas fa-heart"></i> & <i class="fas fa-coffee"></i> in <a target="_blank" href="https://www.google.pl/maps/place/Zabrze/@50.3148562,18.6565665,11z/data=!3m1!4b1!4m5!3m4!1s0x4711327687773cb9:0xc9512bd611a6f0f3!8m2!3d50.3249278!4d18.7857186">Zabrze</a><br/>
			 &copy; <a target="_blank" href="//visualnet.pl">visualnet.pl</a>
			
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
			$( window ).resize(function() {
				  screenh = $( window ).height();
				  footerh = $('.footer').height();
				  containerh = screenh - footerh;
				  $('#container').css('min-height', containerh+'.px');
			});
            
			$(document).ready(function () {

				  screenh = $( window ).height();
				  footerh = $('.footer').height();
				  containerh = screenh - footerh;
				  $('#container').css('min-height', containerh+'.px');		
			
			
                Visualnet.utils = new Visualnet.Utils(); 

                $("#visualnet-monit-cookies").toggle(Visualnet.utils.isCookiesEnabled());
                $("#visualnet-monit-browser").toggle(Visualnet.utils.checkIfBrowser(Visualnet.utils.browser.ie, 8, "="));
            
            
                /**
                 * communication with freshmail API
                 * @example 
                 * 
                 * var data = {
                 * system_url: 'http://www.systembiletowy.pl/dev',
                 *  parameters: {
                 *       email: 'test2@test.pl', 
                 *       wojewodztwo: 'śląskie', 
                 *       miasto: 'Zabrze'
                 *   }
                 *  };
                 *
                 *   $(document).trigger("Visualnet.event.sendFreshMailRequest", [data]); 
                 * 
                 */

                // set event for fresh mail
                Visualnet.utils.setEventFreshMail();
                             
            
            });
            
        </script>  
        
        <!-- monit area / --> 
	<span style="display: none;" class="tooltiptext" id="tooltiptext"></span>
  </body>
</html>
