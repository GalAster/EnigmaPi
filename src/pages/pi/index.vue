<template>
	<div class="page">
		<div class="weui-cells__title">表单</div>
		<div class="weui-cells weui-cells_after-title">
			<div class="weui-cell weui-cell_input">
				<div class="weui-cell__hd">
					<div class="weui-label">qq</div>
				</div>
				<div class="weui-cell__bd">
					<input class="weui-input" placeholder="请输入qq"/>
				</div>
			</div>

			<div class="weui-cell weui-cell_input">
				<div class="weui-cell__hd">
					<div class="weui-label">日期</div>
				</div>
				<div class="weui-cell__bd">
					<picker mode="date" value="date" start="2015-09-01" end="2017-09-01" @change="bindDateChange">
						<div class="weui-input">{{date}}</div>
					</picker>
				</div>
			</div>

			<div class="weui-cells__tips">底部说明文字底部说明文字</div>

			<div class="weui-cells__title">表单报错</div>
			<div class="weui-cells weui-cells_after-title">
				<div class="weui-cell weui-cell_input weui-cell_warn">
					<div class="weui-cell__hd">
						<div class="weui-label">卡号</div>
					</div>
					<div class="weui-cell__bd">
						<input class="weui-input" placeholder="请输入卡号"/>
					</div>
					<div class="weui-cell__ft">
						<icon type="warn" size="23" color="#E64340"></icon>
					</div>
				</div>
			</div>

			<div class="weui-cells weui-cells_after-title">
				<div class="weui-cell weui-cell_switch">
					<div class="weui-cell__bd">本地计算</div>
					<div class="weui-cell__ft">
						<switch checked @change="switchChange"/>
					</div>
				</div>
			</div>
			<rIndex></rIndex>
			<div class="weui-btn-area">
				<button class="weui-btn" type="primary" @click="showTopTipsFun">查询</button>
				<div class="weui-footer weui-footer_fixed-bottom">
				<a href="/pages/index/main">
					<button class="weui-btn" type="warn">返回</button>
				</a>
				</div>
			</div>
		</div>
	</div>
</template>

<script>
	export default {
		data () {
			return {
				showTopTips: false,
				time: '09:01',
				date: '2015-09-01',
				countryCodes: ['+86', '+80', '+84', '+87'],
				countryCodesIndex: 0,
				countries: ['中国', '美国', '英国'],
				countryIndex: 0,
				accounts: ['微信号', 'QQ', 'Email'],
				accountsIndex: 0,

				radioItems: [
					{name: 'cell standard', value: '0'},
					{name: 'cell standard', value: '1', checked: true}
				],
				checkboxItems: [
					{name: 'standard is dealt for u.', value: '1', checked: false},
					{name: 'standard is dealicient for u.', value: '0', checked: true}
				],

				isAgree: false
			}
		},
		methods: {
			switchChange (e) {
				console.log('switch发生change事件，携带value值为：' + e.mp.detail.value);
			},
			bindDateChange (e) {
				this.date = e.mp.detail.value;
				console.log(e.mp.detail.value);
			},
			showTopTipsFun () {
				this.showTopTips = true;
				setTimeout(() => {
					this.showTopTips = false;
				}, 2000)
			}
		}
	}
</script>

<style>

</style>
