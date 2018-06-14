<template>
	<div class="page">
		<!--<div class="page__hd">-->
		<!--<div class="page__title">Enigma π</div>-->
		<!--</div>-->
		<div class="page__bd">
			<!-- 图文组合列表  start -->
			<div class="weui-panel weui-panel_access">
				<div class="weui-panel__hd">寻找你的数字!</div>
				<div class="weui-panel__bd">
					<navigator url="/pages/pi/main" class="weui-media-box weui-media-box_appmsg"
							   hover-class="weui-cell_active">
						<div class="weui-media-box__hd weui-media-box__hd_in-appmsg">
							<image class="weui-media-box__thumb" src="/static/images/Constant Pi.png"></image>
						</div>
						<div class="weui-media-box__bd weui-media-box__bd_in-appmsg">
							<div class="weui-media-box__title">圆周率</div>
							<div class="weui-media-box__desc">由各种物质组成的巨型球状天体，叫做星球。星球有一定的形状，有自己的运行轨道。</div>
						</div>
					</navigator>
					<navigator url="" class="weui-media-box weui-media-box_appmsg" hover-class="weui-cell_active">
						<div class="weui-media-box__hd weui-media-box__hd_in-appmsg">
							<image class="weui-media-box__thumb" src="/static/images/Constant E.png"></image>
						</div>
						<div class="weui-media-box__bd weui-media-box__bd_in-appmsg">
							<div class="weui-media-box__title">自然底数</div>
							<div class="weui-media-box__desc">由各种物质组成的巨型球状天体，叫做星球。星球有一定的形状，有自己的运行轨道。</div>
						</div>
					</navigator>
				</div>
				<navigator url="/pages/more-constants/main" class="weui-panel__ft">
					<div class="weui-cell weui-cell_access weui-cell_link">
						<div class="weui-cell__bd">发现更多常数</div>
						<div class="weui-cell__ft weui-cell__ft_in-access"></div>
					</div>
				</navigator>
			</div>

			<!-- 图文组合列表  end -->

			<!-- 文字组合列表  start -->
			<div class="weui-panel weui-panel_access">
				<div class="weui-panel__hd">推荐阅读</div>
				<div class="weui-panel__bd">
					<navigator url="" class="weui-media-box weui-media-box_text">
						<div class="weui-media-box__title weui-media-box__title_in-text">标题一</div>
						<div class="weui-media-box__desc">由各种物质组成的巨型球状天体，叫做星球。星球有一定的形状，有自己的运行轨道。</div>
					</navigator>
					<navigator url="" class="weui-media-box weui-media-box_text">
						<div class="weui-media-box__title weui-media-box__title_in-text">标题二</div>
						<div class="weui-media-box__desc">由各种物质组成的巨型球状天体，叫做星球。星球有一定的形状，有自己的运行轨道。</div>
						<div class="weui-media-box__info">
							<div class="weui-media-box__info__meta">文字来源</div>
							<div class="weui-media-box__info__meta">时间</div>
							<div class="weui-media-box__info__meta weui-media-box__info__meta_extra">其它信息</div>
						</div>
					</navigator>
				</div>
				<navigator url="/pages/more-articles/main" class="weui-panel__ft">
					<div class="weui-cell weui-cell_access weui-cell_link">
						<div class="weui-cell__bd">更多有趣文章</div>
						<div class="weui-cell__ft weui-cell__ft_in-access"></div>
					</div>
				</navigator>
			</div>
			<!-- 文字组合列表  end -->

			<!-- 小图文组合列表  start -->
			<!--<div class="weui-panel">-->
			<!--<div class="weui-panel__hd">小游戏</div>-->
			<!--<div class="weui-panel__bd">-->
			<!--<div class="weui-media-box weui-media-box_small-appmsg">-->
			<!--<div class="weui-cells weui-cells_in-small-appmsg">-->
			<!--<navigator url="" class="weui-cell weui-cell_access" hover-class="weui-cell_active">-->
			<!--<div class="weui-cell__hd">-->
			<!--<image :src="icon20" style="width: 20px;height: 20px;margin-right: 5px"/>-->
			<!--</div>-->
			<!--<div class="weui-cell__bd weui-cell_primary">-->
			<!--<div>敬请期待</div>-->
			<!--</div>-->
			<!--<div class="weui-cell__ft weui-cell__ft_in-access"></div>-->
			<!--</navigator>-->
			<!--<navigator url="" class="weui-cell weui-cell_access" hover-class="weui-cell_active">-->
			<!--<div class="weui-cell__hd">-->
			<!--<image :src="icon20" style="width: 20px;height: 20px;margin-right: 5px"/>-->
			<!--</div>-->
			<!--<div class="weui-cell__bd weui-cell_primary">-->
			<!--<div>敬请期待</div>-->
			<!--</div>-->
			<!--<div class="weui-cell__ft weui-cell__ft_in-access"></div>-->
			<!--</navigator>-->
			<!--</div>-->
			<!--</div>-->
			<!--</div>-->
			<!--<navigator url="" class="weui-panel__ft">-->
			<!--<div class="weui-cell weui-cell_access weui-cell_link">-->
			<!--<div class="weui-cell__bd">查看更多</div>-->
			<!--<div class="weui-cell__ft weui-cell__ft_in-access"></div>-->
			<!--</div>-->
			<!--</navigator>-->
			<!--</div>-->
			<!-- 小图文组合列表 end -->
			<!--<button class="weui-btn" type="warn">退出应用</button>-->
			<!--<br>-->
			<!--<hr style="border:1 dashed #987cb9" width="80%" color=#987cb9 size=1>-->
			<!--<br>-->
			<div class="weui-footer">
				<!--<div class="weui-footer__links">-->
				<!--<navigator url="" class="weui-footer__link">底部链接</navigator>-->
				<!--<navigator url="" class="weui-footer__link">底部链接</navigator>-->
				<!--</div>-->
				<div class="weui-footer__text">Copyright © 2018-2018 0x0.life</div>
			</div>

			<!--<div class="weui-footer weui-footer_fixed-bottom">-->
			<!--<div class="weui-footer__links">-->
			<!--<navigator url="" class="weui-footer__link">WeUI首页</navigator>-->
			<!--</div>-->
			<!--<div class="weui-footer__text">Copyright © 2008-2018 weui.io</div>-->
			<!--</div>-->
		</div>
	</div>
</template>

<script>

</script>

<style>
	/* 作为展示用的样式 */

	.weui-footer {
		margin-bottom: 50px;
	}

	/*.weui-footer_fixed-bottom {*/
	/*margin-bottom: 0;*/
	/*}*/

	/*hr {*/
	/*border-top: 1px dashed #8c8b8b;*/
	/*}*/

	/*hr:after {*/
	/*content: '\002702';*/
	/*display: inline-block;*/
	/*position: relative;*/
	/*top: -16px;*/
	/*left: 40px;*/
	/*padding: 0 3px;*/
	/*background: #f0f0f000;*/
	/*color: #8c8b8b;*/
	/*font-size: 18px;*/
	/*}*/
</style>
