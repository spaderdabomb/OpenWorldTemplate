// Amplify Impostors
// Copyright (c) Amplify Creations, Lda <info@amplify.pt>

Shader "Hidden/Amplify Impostors/Octahedron Impostor URP"
{
	Properties
	{
		[NoScaleOffset]_Albedo("Albedo & Alpha", 2D) = "white" {}
		[NoScaleOffset]_Normals("Normals & Depth", 2D) = "white" {}
		[NoScaleOffset]_Specular("Specular & Smoothness", 2D) = "black" {}
		[NoScaleOffset]_Emission("Emission & Occlusion", 2D) = "black" {}
		[HideInInspector]_Frames("Frames", Float) = 16
		[HideInInspector]_ImpostorSize("Impostor Size", Float) = 1
		[HideInInspector]_Offset("Offset", Vector) = (0,0,0,0)
		[HideInInspector]_AI_SizeOffset( "Size & Offset", Vector ) = ( 0,0,0,0 )
		_TextureBias("Texture Bias", Float) = -1
		_Parallax("Parallax", Range( -1 , 1)) = 1
		[HideInInspector]_DepthSize("DepthSize", Float) = 1
		_ClipMask("Clip", Range( 0 , 1)) = 0.5
		_AI_ShadowBias( "Shadow Bias", Range( 0 , 2)) = 0.25
		_AI_ShadowView( "Shadow View", Range( 0 , 1 ) ) = 1
		[Toggle(_HEMI_ON)] _Hemi("Hemi", Float) = 0
		[Toggle(EFFECT_HUE_VARIATION)] _Hue("Use SpeedTree Hue", Float) = 0
		_HueVariation("Hue Variation", Color) = (0,0,0,0)
		[Toggle] _AI_AlphaToCoverage("Alpha To Coverage", Float) = 0
	}

	SubShader
	{
		Tags { "RenderPipeline" = "UniversalPipeline" "RenderType" = "Opaque" "Queue" = "Geometry" "DisableBatching" = "True" }

		Cull Back
		AlphaToMask[_AI_AlphaToCoverage]

		HLSLINCLUDE
			#pragma target 3.0
			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			struct SurfaceOutputStandardSpecular
			{
				half3 Albedo;
				half3 Specular;
				float3 Normal;
				half3 Emission;
				half Smoothness;
				half Occlusion;
				half Alpha;
			};
		ENDHLSL

		Pass
		{
			Name "Forward"
			Tags{"LightMode" = "UniversalForward"}

			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA

			HLSLPROGRAM
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile_fragment _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION
			#pragma multi_compile_fragment _ _SHADOWS_SOFT
			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile_fragment _ _LIGHT_LAYERS
			#pragma multi_compile_fragment _ _LIGHT_COOKIES
			#pragma multi_compile _ _FORWARD_PLUS

			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK
            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON

			#pragma multi_compile_fog
			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE

			#pragma vertex vert
			#pragma fragment frag

			#define _SPECULAR_SETUP 1

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON
			#pragma shader_feature EFFECT_HUE_VARIATION

			struct VertexInput
			{
				float4 vertex    : POSITION;
				float3 normal    : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos                : SV_POSITION;
				float4 uvsFrame1              : TEXCOORD0;
				float4 uvsFrame2              : TEXCOORD1;
				float4 uvsFrame3              : TEXCOORD2;
				float4 octaFrame              : TEXCOORD3;
				float4 viewPos                : TEXCOORD4;
				float4 fogFactorAndVertexLight : TEXCOORD5;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert ( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.vertex.xyz);

				float3 normalWS = TransformObjectToWorldNormal( v.normal );
				half3 vertexLight = VertexLighting(vertexInput.positionWS, normalWS);
				half fogFactor = ComputeFogFactor(vertexInput.positionCS.z);

				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				o.clipPos = vertexInput.positionCS;
				return o;
			}

			half4 frag (VertexOutput IN, out float outDepth : SV_Depth ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				InputData inputData;
				inputData.positionCS = IN.clipPos;
				inputData.positionWS = worldPos;
				inputData.normalWS = o.Normal;
				inputData.viewDirectionWS = SafeNormalize( _WorldSpaceCameraPos.xyz - worldPos );
				inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				inputData.bakedGI = SampleSH( inputData.normalWS );
				inputData.shadowMask = 1; // not supported
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV( IN.clipPos );
				#if defined(_MAIN_LIGHT_SHADOWS)
					inputData.shadowCoord = TransformWorldToShadowCoord(inputData.positionWS);
				#else
					inputData.shadowCoord = float4(0, 0, 0, 0);
				#endif

				half4 color = UniversalFragmentPBR(
					inputData,
					o.Albedo,
					0,
					o.Specular,
					o.Smoothness,
					o.Occlusion,
					o.Emission,
					o.Alpha);

				color.rgb = MixFog( color.rgb, IN.fogFactorAndVertexLight.x );
				outDepth = clipPos.z;
				return color;
			}

			ENDHLSL
		}

		Pass
		{
			Name "GBuffer"
			Tags{"LightMode" = "UniversalGBuffer"}

			ZWrite On
			ZTest LEqual
			Cull Back

			HLSLPROGRAM

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION
			#pragma multi_compile_fragment _ _SHADOWS_SOFT
			#pragma multi_compile_fragment _ _LIGHT_LAYERS
			#pragma multi_compile_fragment _ _RENDER_PASS_ENABLED

			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT

			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK
            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON

			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE

			#pragma vertex vert
			#pragma fragment frag

			#define _SPECULAR_SETUP 1

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"
			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON
			#pragma shader_feature EFFECT_HUE_VARIATION

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos     : SV_POSITION;
				float4 uvsFrame1   : TEXCOORD0;
				float4 uvsFrame2   : TEXCOORD1;
				float4 uvsFrame3   : TEXCOORD2;
				float4 octaFrame   : TEXCOORD3;
				float4 viewPos     : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.vertex.xyz);

				o.clipPos = vertexInput.positionCS;
				return o;
			}

			FragmentOutput frag( VertexOutput IN, out float outDepth : SV_Depth )
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				SurfaceOutputStandardSpecular o = ( SurfaceOutputStandardSpecular )0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				InputData inputData;
				inputData.positionCS = IN.clipPos;
				inputData.positionWS = worldPos;
				inputData.normalWS = o.Normal;
				inputData.viewDirectionWS = SafeNormalize( _WorldSpaceCameraPos.xyz - worldPos );
				inputData.bakedGI = SampleSH( inputData.normalWS );
				inputData.shadowMask = 1; // not supported
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV( IN.clipPos );
				#if defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					inputData.shadowCoord = TransformWorldToShadowCoord(inputData.positionWS);
				#else
					inputData.shadowCoord = float4(0, 0, 0, 0);
				#endif

				BRDFData brdfData;
				InitializeBRDFData( o.Albedo, 0, o.Specular, o.Smoothness, o.Alpha, brdfData );

				Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, inputData.shadowMask);
				MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, inputData.shadowMask);
				half3 color = GlobalIllumination(brdfData, inputData.bakedGI, o.Occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
				outDepth = clipPos.z;
				return BRDFDataToGbuffer(brdfData, inputData, o.Smoothness, o.Emission + color, o.Occlusion);
			}


			ENDHLSL
		}


		Pass
		{
			Name "ShadowCaster"
			Tags { "LightMode" = "ShadowCaster" }

			ZWrite On
			ZTest LEqual

			HLSLPROGRAM
			#ifndef UNITY_PASS_SHADOWCASTER
			#define UNITY_PASS_SHADOWCASTER
			#endif

            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON
			#pragma multi_compile_instancing
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normal : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos   : SV_POSITION;
				float4 uvsFrame1 : TEXCOORD0;
				float4 uvsFrame2 : TEXCOORD1;
				float4 uvsFrame3 : TEXCOORD2;
				float4 octaFrame : TEXCOORD3;
				float4 viewPos   : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				o.clipPos = TransformObjectToHClip( v.vertex.xyz );

				return o;
			}

			half4 frag( VertexOutput IN, out float outDepth : SV_Depth ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);
				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				outDepth = clipPos.z;
				return 0;
			}
			ENDHLSL
		}

		Pass
		{
			Name "DepthOnly"
			Tags{ "LightMode" = "DepthOnly" }

			ZWrite On
			ColorMask 0

			HLSLPROGRAM
            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON
			#pragma multi_compile_instancing
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON

			struct VertexInput
			{
				float4 vertex   : POSITION;
				float3 normal   : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos   : SV_POSITION;
				float4 uvsFrame1 : TEXCOORD0;
				float4 uvsFrame2 : TEXCOORD1;
				float4 uvsFrame3 : TEXCOORD2;
				float4 octaFrame : TEXCOORD3;
				float4 viewPos   : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				o.clipPos = TransformObjectToHClip( v.vertex.xyz );

				return o;
			}

			half4 frag( VertexOutput IN, out float outDepth : SV_Depth ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);
				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				outDepth = clipPos.z;
				return 0;
			}

			ENDHLSL
		}

		Pass
		{
			Name "DepthNormals"
			Tags{ "LightMode" = "DepthNormals" }

			ZWrite On
			ColorMask RGBA

			HLSLPROGRAM
            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON
			#pragma multi_compile_instancing
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma multi_compile_fragment _ LOD_FADE_CROSSFADE

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON

			struct VertexInput
			{
				float4 vertex   : POSITION;
				float3 normal   : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos   : SV_POSITION;
				float4 uvsFrame1 : TEXCOORD0;
				float4 uvsFrame2 : TEXCOORD1;
				float4 uvsFrame3 : TEXCOORD2;
				float4 octaFrame : TEXCOORD3;
				float4 viewPos   : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				o.clipPos = TransformObjectToHClip( v.vertex.xyz );

				return o;
			}

			half4 frag( VertexOutput IN, out float outDepth : SV_Depth ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);
				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.clipPos );
				#endif

				outDepth = clipPos.z;
				return half4( NormalizeNormalPerPixel( o.Normal ), 0.0);
			}

			ENDHLSL
		}

		Pass
		{
			Name "SceneSelectionPass"
			Tags{ "LightMode" = "SceneSelectionPass" }

			ZWrite On
			ColorMask 0

			HLSLPROGRAM
            #pragma multi_compile _ UNITY_SINGLE_PASS_STEREO STEREO_INSTANCING_ON STEREO_MULTIVIEW_ON
			#pragma multi_compile_instancing

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			int _ObjectId;
			int _PassValue;

			struct VertexInput
			{
				float4 vertex   : POSITION;
				float3 normal   : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos   : SV_POSITION;
				float4 uvsFrame1 : TEXCOORD0;
				float4 uvsFrame2 : TEXCOORD1;
				float4 uvsFrame3 : TEXCOORD2;
				float4 octaFrame : TEXCOORD3;
				float4 viewPos   : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				o.clipPos = TransformObjectToHClip( v.vertex.xyz );

				return o;
			}

			half4 frag( VertexOutput IN, out float outDepth : SV_Depth ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);
				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				outDepth = IN.clipPos.z;
				return float4( _ObjectId, _PassValue, 1.0, 1.0 );
			}

			ENDHLSL
		}

		Pass
		{
			Name "Meta"
			Tags{"LightMode" = "Meta"}

			Cull Off

			HLSLPROGRAM
			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"

			#define AI_RENDERPIPELINE

			#include "AmplifyImpostors.cginc"

			#pragma shader_feature _HEMI_ON
			#pragma shader_feature EFFECT_HUE_VARIATION

			struct VertexInput
			{
				float4 vertex   : POSITION;
				float3 normal   : NORMAL;
				float2 uvLM     : TEXCOORD1;
				float2 uvDLM    : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos   : SV_POSITION;
				float4 uvsFrame1 : TEXCOORD0;
				float4 uvsFrame2 : TEXCOORD1;
				float4 uvsFrame3 : TEXCOORD2;
				float4 octaFrame : TEXCOORD3;
				float4 viewPos   : TEXCOORD4;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			VertexOutput vert( VertexInput v )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				OctaImpostorVertex( v.vertex, v.normal, o.uvsFrame1, o.uvsFrame2, o.uvsFrame3, o.octaFrame, o.viewPos );

				o.clipPos = MetaVertexPosition( v.vertex, v.uvLM, v.uvDLM, unity_LightmapST, unity_DynamicLightmapST );
				return o;
			}

			half4 frag( VertexOutput IN, out float outDepth : SV_Depth ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);
				SurfaceOutputStandardSpecular o = (SurfaceOutputStandardSpecular)0;
				float4 clipPos = 0;
				float3 worldPos = 0;

				OctaImpostorFragment( o, clipPos, worldPos, IN.uvsFrame1, IN.uvsFrame2, IN.uvsFrame3, IN.octaFrame, IN.viewPos );
				IN.clipPos.zw = clipPos.zw;

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = o.Albedo;
				metaInput.Emission = o.Emission;

				outDepth = clipPos.z;

				return MetaFragment( metaInput );
			}
			ENDHLSL
		}
	}
	FallBack "Hidden/InternalErrorShader"
}
