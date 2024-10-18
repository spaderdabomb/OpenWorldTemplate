Shader /*ase_name*/ "Hidden/Impostors/Lit URP" /*end*/
{
    Properties
    {
		/*ase_props*/
		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 1.0
		//_TransStrength( "Trans Strength", Range( 0, 16 ) ) = 2
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.0
		//_TransScattering( "Trans Scattering", Range( 0, 16 ) ) = 8
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 1.0
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.2
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 1.0
    }

    SubShader
    {
		/*ase_subshader_options:Name=Additional Options
			Option:Material Type,InvertActionOnDeselection:Standard,Specular Color:Specular Color
				Standard:ShowPort:Metallic
				Standard:HidePort:Specular
				Standard:RemoveDefine:_SPECULAR_SETUP 1
				Specular Color:ShowPort:Specular
				Specular Color:HidePort:Metallic
				Specular Color:SetDefine:_SPECULAR_SETUP 1
			Option:Receive Shadows:false,true:true
				true:RemoveDefine:_RECEIVE_SHADOWS_OFF 1
				false:SetDefine:_RECEIVE_SHADOWS_OFF 1
			Port:Base:Alpha Clip Threshold
				On:SetDefine:_ALPHATEST_ON 1
			Option:Transmission:false,true:false
				true:SetPropertyOnPass:Base:ChangeTagValue,LightMode,UniversalForwardOnly
				true:SetPropertyOnPass:DepthNormals:ChangeTagValue,LightMode,DepthNormalsOnly
				true:ExcludePass:GBuffer
				false,disable:SetPropertyOnPass:Base:ChangeTagValue,LightMode,UniversalForward
				false,disable:SetPropertyOnPass:DepthNormals:ChangeTagValue,LightMode,DepthNormals
				false,disable:IncludePass:GBuffer
				false:RemoveDefine:ASE_TRANSMISSION 1
				false:HidePort:Base:Transmission
				false:HideOption:  Transmission Shadow
				true:SetDefine:ASE_TRANSMISSION 1
				true:ShowPort:Base:Transmission
				true:ShowOption:  Transmission Shadow
			Field:  Transmission Shadow:Float:0.5:0:1:_TransmissionShadow
				Change:SetMaterialProperty:_TransmissionShadow
				Change:SetShaderProperty:_TransmissionShadow,_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
				Inline,disable:SetShaderProperty:_TransmissionShadow,//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
			Option:Translucency:false,true:false
				true:SetPropertyOnPass:Base:ChangeTagValue,LightMode,UniversalForwardOnly
				true:SetPropertyOnPass:DepthNormals:ChangeTagValue,LightMode,DepthNormalsOnly
				true:ExcludePass:GBuffer
				false,disable:SetPropertyOnPass:Base:ChangeTagValue,LightMode,UniversalForward
				false,disable:SetPropertyOnPass:DepthNormals:ChangeTagValue,LightMode,DepthNormals
				false,disable:IncludePass:GBuffer
				false:RemoveDefine:ASE_TRANSLUCENCY 1
				false:HidePort:Base:Translucency
				false:HideOption:  Translucency Strength
				false:HideOption:  Normal Distortion
				false:HideOption:  Scattering
				false:HideOption:  Direct
				false:HideOption:  Ambient
				false:HideOption:  Shadow
				true:SetDefine:ASE_TRANSLUCENCY 1
				true:ShowPort:Base:Translucency
				true:ShowOption:  Translucency Strength
				true:ShowOption:  Normal Distortion
				true:ShowOption:  Scattering
				true:ShowOption:  Direct
				true:ShowOption:  Ambient
				true:ShowOption:  Shadow
			Field:  Translucency Strength:Float:1:0:50:_TransStrength
				Change:SetMaterialProperty:_TransStrength
				Change:SetShaderProperty:_TransStrength,_TransStrength( "Strength", Range( 0, 50 ) ) = 1
				Inline,disable:SetShaderProperty:_TransStrength,//_TransStrength( "Strength", Range( 0, 50 ) ) = 1
			Field:  Normal Distortion:Float:0.5:0:1:_TransNormal
				Change:SetMaterialProperty:_TransNormal
				Change:SetShaderProperty:_TransNormal,_TransNormal( "Normal Distortion", Range( 0, 1 ) ) = 0.5
				Inline,disable:SetShaderProperty:_TransNormal,//_TransNormal( "Normal Distortion", Range( 0, 1 ) ) = 0.5
			Field:  Scattering:Float:2:1:50:_TransScattering
				Change:SetMaterialProperty:_TransScattering
				Change:SetShaderProperty:_TransScattering,_TransScattering( "Scattering", Range( 1, 50 ) ) = 2
				Inline,disable:SetShaderProperty:_TransScattering,//_TransScattering( "Scattering", Range( 1, 50 ) ) = 2
			Field:  Direct:Float:0.9:0:1:_TransDirect
				Change:SetMaterialProperty:_TransDirect
				Change:SetShaderProperty:_TransDirect,_TransDirect( "Direct", Range( 0, 1 ) ) = 0.9
				Inline,disable:SetShaderProperty:_TransDirect,//_TransDirect( "Direct", Range( 0, 1 ) ) = 0.9
			Field:  Ambient:Float:0.1:0:1:_TransAmbient
				Change:SetMaterialProperty:_TransAmbient
				Change:SetShaderProperty:_TransAmbient,_TransAmbient( "Ambient", Range( 0, 1 ) ) = 0.1
				Inline,disable:SetShaderProperty:_TransAmbient,//_TransAmbient( "Ambient", Range( 0, 1 ) ) = 0.1
			Field:  Shadow:Float:0.5:0:1:_TransShadow
				Change:SetMaterialProperty:_TransShadow
				Change:SetShaderProperty:_TransShadow,_TransShadow( "Shadow", Range( 0, 1 ) ) = 0.5
				Inline,disable:SetShaderProperty:_TransShadow,//_TransShadow( "Shadow", Range( 0, 1 ) ) = 0.5
			Option:LOD CrossFade:false,true:true
				true:SetDefine:Base:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				true:SetDefine:GBuffer:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				true:SetDefine:ShadowCaster:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				true:SetDefine:DepthOnly:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				true:SetDefine:DepthNormals:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				false:RemoveDefine:Base:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				false:RemoveDefine:GBuffer:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				false:RemoveDefine:ShadowCaster:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				false:RemoveDefine:DepthOnly:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
				false:RemoveDefine:DepthNormals:pragma multi_compile_fragment _ LOD_FADE_CROSSFADE
			Option:DOTS Instancing:false,true:false
				true:SetDefine:Base:pragma multi_compile _ DOTS_INSTANCING_ON
				true:SetDefine:GBuffer:pragma multi_compile _ DOTS_INSTANCING_ON
				true:SetDefine:ShadowCaster:pragma multi_compile _ DOTS_INSTANCING_ON
				true:SetDefine:DepthOnly:pragma multi_compile _ DOTS_INSTANCING_ON
				true:SetDefine:DepthNormals:pragma multi_compile _ DOTS_INSTANCING_ON
				false:RemoveDefine:Base:pragma multi_compile _ DOTS_INSTANCING_ON
				false:RemoveDefine:GBuffer:pragma multi_compile _ DOTS_INSTANCING_ON
				false:RemoveDefine:ShadowCaster:pragma multi_compile _ DOTS_INSTANCING_ON
				false:RemoveDefine:DepthOnly:pragma multi_compile _ DOTS_INSTANCING_ON
				false:RemoveDefine:DepthNormals:pragma multi_compile _ DOTS_INSTANCING_ON
		*/
        Tags
		{
			"RenderPipeline" = "UniversalPipeline"
			"RenderType"="Opaque"
			"Queue"="Geometry"
			"UniversalMaterialType" = "Lit"
		}

		Cull Back
		AlphaToMask Off

		HLSLINCLUDE
		#pragma target 3.5
		#pragma prefer_hlslcc gles
		#pragma exclude_renderers d3d9 // ensure rendering platforms toggle list is visible

		struct SurfaceOutput
		{
			half3 Albedo;
			half3 Specular;
			half Metallic;
			float3 Normal;
			half3 Emission;
			half Smoothness;
			half Occlusion;
			half Alpha;
		};

		#define AI_RENDERPIPELINE
		ENDHLSL

		/*ase_pass*/
        Pass
        {
			/*ase_main_pass*/
			Name "Base"
        	Tags{"LightMode" = "UniversalForward"}

			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA
            /*ase_stencil*/

        	HLSLPROGRAM

			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile_fog

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile_fragment _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION
			#pragma multi_compile_fragment _ _SHADOWS_SOFT
			#pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
			//#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile_fragment _ _LIGHT_LAYERS
			#pragma multi_compile_fragment _ _LIGHT_COOKIES
			#pragma multi_compile _ _FORWARD_PLUS
			//#pragma shader_feature_local _RECEIVE_SHADOWS_OFF
			//#pragma shader_feature_local_fragment _SPECULARHIGHLIGHTS_OFF
			//#pragma shader_feature_local_fragment _ENVIRONMENTREFLECTIONS_OFF

			//#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			//#pragma multi_compile _ SHADOWS_SHADOWMASK
			//#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			//#pragma multi_compile _ LIGHTMAP_ON
			//#pragma multi_compile _ DYNAMICLIGHTMAP_ON
			//#pragma multi_compile_fragment _ DEBUG_DISPLAY
			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			/*ase_pragma*/

            struct VertexInput
            {
                float4 positionOS : POSITION;
                float3 normalOS : NORMAL;
                float4 tangentOS : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				/*ase_vdata:p=p;n=n;t=t;uv0=tc0.xyzw;uv1=tc1.xyzw*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
            {
                float4 positionCS                : SV_POSITION;
                float4 lightmapUVOrVertexSH	  : TEXCOORD0;
        		half4 fogFactorAndVertexLight : TEXCOORD1;
				/*ase_interp(3,):sp=sp.xyzw;*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
            	UNITY_VERTEX_OUTPUT_STEREO
            };

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

            VertexOutput vert(VertexInput v/*ase_vert_input*/)
        	{
        		VertexOutput o = (VertexOutput)0;
                UNITY_SETUP_INSTANCE_ID(v);
            	UNITY_TRANSFER_INSTANCE_ID(v, o);
        		UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				v.positionOS.xyz += /*ase_vert_out:Vertex Offset;Float3;8;-1;_Vertex*/ float3( 0, 0, 0 ) /*end*/;

        		// Vertex shader outputs defined by graph
                float3 lwWNormal = TransformObjectToWorldNormal(v.normalOS );

                VertexPositionInputs vertexInput = GetVertexPositionInputs(v.positionOS.xyz);

         		// We either sample GI from lightmap or SH.
        	    // Lightmap UV and vertex SH coefficients use the same interpolator ("float2 lightmapUV" for lightmap or "half3 vertexSH" for SH)
                // see DECLARE_LIGHTMAP_OR_SH macro.
        	    // The following funcions initialize the correct variable with correct data
        	    OUTPUT_LIGHTMAP_UV(v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy);
        	    OUTPUT_SH(lwWNormal, o.lightmapUVOrVertexSH.xyz);

        	    half3 vertexLight = VertexLighting(vertexInput.positionWS, lwWNormal);
        	    half fogFactor = ComputeFogFactor(vertexInput.positionCS.z);
        	    o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
        	    o.positionCS = vertexInput.positionCS;

        		return o;
        	}

        	half4 frag (VertexOutput IN
						, out float outDepth : SV_Depth
						#ifdef _WRITE_RENDERING_LAYERS
						, out float4 outRenderingLayers : SV_Target1
						#endif
						/*ase_frag_input*/) : SV_Target			
			{
            	UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;

				//Calculated by AI
				float4 positionCS = 0;
				float3 positionWS = 0;

				/*ase_frag_code:IN=VertexOutput*/

				float3 Albedo = /*ase_frag_out:Base Color;Float3;0;-1;_BaseColor*/float3( 0.5, 0.5, 0.5 )/*end*/;
				float3 Normal = /*ase_frag_out:World Normal;Float3;1;-1;_FragNormal*/float3( 0, 0, 1 )/*end*/;
				float3 Emission = /*ase_frag_out:Emission;Float3;2;-1;_Emission*/0/*end*/;
				float3 Specular = /*ase_frag_out:Specular;Float3;9;-1;_Specular*/0.5/*end*/;
				float Metallic = /*ase_frag_out:Metallic;Float;3;-1;_Metallic*/0/*end*/;
				float Smoothness = /*ase_frag_out:Smoothness;Float;4;-1;_Smoothness*/0.5/*end*/;
				float Occlusion = /*ase_frag_out:Occlusion;Float;5;-1;_Occlusion*/1/*end*/;
				float Alpha = /*ase_frag_out:Alpha;Float;6;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;7;-1;_AlphaClip*/0.5/*end*/;
				float3 BakedGI = /*ase_frag_out:Baked GI;Float3;11;-1;_BakedGI*/0/*end*/;
				float3 Transmission = /*ase_frag_out:Transmission;Float3;14;-1;_Transmission*/1/*end*/;
				float3 Translucency = /*ase_frag_out:Translucency;Float3;15;-1;_Translucency*/1/*end*/;

				IN.positionCS.zw = positionCS.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				float3 WorldSpaceViewDirection = SafeNormalize( _WorldSpaceCameraPos.xyz - positionWS );

				InputData inputData = (InputData)0;
        		inputData.positionWS = positionWS;
				inputData.normalWS = Normal;
				inputData.viewDirectionWS = normalize(WorldSpaceViewDirection);

				#if defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					inputData.shadowCoord = TransformWorldToShadowCoord( positionWS );
				#endif

        	    inputData.fogCoord = IN.fogFactorAndVertexLight.x;
        	    inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.positionCS);
				inputData.shadowMask = 1;

				#if defined(CUSTOM_BAKED_GI)
					half4 decodeInstructions = half4( LIGHTMAP_HDR_MULTIPLIER, LIGHTMAP_HDR_EXPONENT, 0.0h, 0.0h );
					inputData.bakedGI = UnpackLightmapRGBM( BakedGI, decodeInstructions ) * EMISSIVE_RGBM_SCALE;
				#else
					inputData.bakedGI = SAMPLE_GI(IN.lightmapUVOrVertexSH.xy, IN.lightmapUVOrVertexSH.xyz, inputData.normalWS);
				#endif

				SurfaceData surfaceData;
				surfaceData.albedo = Albedo;
				surfaceData.metallic = Metallic;
				surfaceData.specular = Specular;
				surfaceData.smoothness = Smoothness,
				surfaceData.occlusion = Occlusion,
				surfaceData.emission = Emission,
				surfaceData.alpha = Alpha;
				surfaceData.normalTS = Normal;
				surfaceData.clearCoatMask = 0;
				surfaceData.clearCoatSmoothness = 1;

				#ifdef _DBUFFER
					ApplyDecalToSurfaceData(IN.positionCS, surfaceData, inputData);
				#endif

				half4 color = UniversalFragmentPBR(inputData, surfaceData);

				#ifdef ASE_TRANSMISSION
				{
					float shadow = /*ase_inline_begin*/_TransmissionShadow/*ase_inline_end*/;

					#define SUM_LIGHT_TRANSMISSION(Light)\
						float3 atten = Light.color * Light.distanceAttenuation;\
						atten = lerp( atten, atten * Light.shadowAttenuation, shadow );\
						half3 transmission = max( 0, -dot( inputData.normalWS, Light.direction ) ) * atten * Transmission;\
						color.rgb += Albedo * transmission;

					SUM_LIGHT_TRANSMISSION(GetMainLight(inputData.shadowCoord));

					#if defined(_ADDITIONAL_LIGHTS)
						uint meshRenderingLayers = GetMeshRenderingLayer();
						uint pixelLightCount = GetAdditionalLightsCount();
						#if USE_FORWARD_PLUS
						for (uint lightIndex = 0; lightIndex < min(URP_FP_DIRECTIONAL_LIGHTS_COUNT, MAX_VISIBLE_LIGHTS); lightIndex++)
						{
							FORWARD_PLUS_SUBTRACTIVE_LIGHT_CHECK

							Light light = GetAdditionalLight(lightIndex, inputData.positionWS);
							#ifdef _LIGHT_LAYERS
							if (IsMatchingLightLayer(light.layerMask, meshRenderingLayers))
							#endif
							{
								SUM_LIGHT_TRANSMISSION(light);
							}
						}
						#endif
						LIGHT_LOOP_BEGIN(pixelLightCount)
							Light light = GetAdditionalLight(lightIndex, inputData.positionWS);
							#ifdef _LIGHT_LAYERS
							if (IsMatchingLightLayer(light.layerMask, meshRenderingLayers))
							#endif
							{
								SUM_LIGHT_TRANSMISSION(light);
							}
						LIGHT_LOOP_END
					#endif
				}
				#endif

				#ifdef ASE_TRANSLUCENCY
				{
					float shadow = /*ase_inline_begin*/_TransShadow/*ase_inline_end*/;
					float normal = /*ase_inline_begin*/_TransNormal/*ase_inline_end*/;
					float scattering = /*ase_inline_begin*/_TransScattering/*ase_inline_end*/;
					float direct = /*ase_inline_begin*/_TransDirect/*ase_inline_end*/;
					float ambient = /*ase_inline_begin*/_TransAmbient/*ase_inline_end*/;
					float strength = /*ase_inline_begin*/_TransStrength/*ase_inline_end*/;

					#define SUM_LIGHT_TRANSLUCENCY(Light)\
						float3 atten = Light.color * Light.distanceAttenuation;\
						atten = lerp( atten, atten * Light.shadowAttenuation, shadow );\
						half3 lightDir = Light.direction + inputData.normalWS * normal;\
						half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );\
						half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;\
						color.rgb += Albedo * translucency * strength;

					SUM_LIGHT_TRANSLUCENCY(GetMainLight(inputData.shadowCoord));

					#if defined(_ADDITIONAL_LIGHTS)
						uint meshRenderingLayers = GetMeshRenderingLayer();
						uint pixelLightCount = GetAdditionalLightsCount();
						#if USE_FORWARD_PLUS
							for (uint lightIndex = 0; lightIndex < min(URP_FP_DIRECTIONAL_LIGHTS_COUNT, MAX_VISIBLE_LIGHTS); lightIndex++)
							{
								FORWARD_PLUS_SUBTRACTIVE_LIGHT_CHECK

								Light light = GetAdditionalLight(lightIndex, inputData.positionWS);
								#ifdef _LIGHT_LAYERS
								if (IsMatchingLightLayer(light.layerMask, meshRenderingLayers))
								#endif
								{
									SUM_LIGHT_TRANSLUCENCY(light);
								}
							}
						#endif
						LIGHT_LOOP_BEGIN(pixelLightCount)
							Light light = GetAdditionalLight(lightIndex, inputData.positionWS);
							#ifdef _LIGHT_LAYERS
							if (IsMatchingLightLayer(light.layerMask, meshRenderingLayers))
							#endif
							{
								SUM_LIGHT_TRANSLUCENCY(light);
							}
						LIGHT_LOOP_END
					#endif
				}
				#endif

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);

				outDepth = IN.positionCS.z;

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4(EncodeMeshRenderingLayer(renderingLayers), 0, 0, 0);
				#endif

        		return color;
            }

        	ENDHLSL
        }

		/*ase_pass*/
        Pass
        {
			/*ase_hide_pass*/
        	Name "ShadowCaster"
            Tags{"LightMode" = "ShadowCaster"}

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

            HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_instancing

			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			/*ase_pragma*/

            struct VertexInput
            {
                float4 positionOS : POSITION;
                float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				/*ase_vdata:p=p;n=n;uv0=tc0.xyzw*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 positionCS : SV_POSITION;
                /*ase_interp(7,):sp=sp.xyzw;*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
        	};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

			float3 _LightDirection;
			float3 _LightPosition;

            VertexOutput vert(VertexInput v/*ase_vert_input*/)
        	{
        	    VertexOutput o;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				v.positionOS.xyz += /*ase_vert_out:Vertex Offset;Float3;2;-1;_Vertex*/ float3(0,0,0) /*end*/;

        	    float3 positionWS = TransformObjectToWorld(v.positionOS.xyz);
                float3 normalWS = TransformObjectToWorldDir(v.normalOS );

                //float invNdotL = 1.0 - saturate(dot(_LightDirection, normalWS));
                //float scale = invNdotL * _ShadowBias.y;

                // normal bias is negative since we want to apply an inset normal offset
				//positionWS = _LightDirection * _ShadowBias.xxx + positionWS;
				//positionWS = normalWS * scale.xxx + positionWS;

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
				float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
				float3 lightDirectionWS = _LightDirection;
				#endif

				float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
				positionCS.z = min(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#else
				positionCS.z = max(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#endif

				// no need for shadow bias alteration since we do it in fragment anyway
				o.positionCS = positionCS;

        	    return o;
        	}

            half4 frag(VertexOutput IN, out float outDepth : SV_Depth /*ase_frag_input*/) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;

				/*ase_frag_code:IN=GraphVertexOutput*/

				IN.positionCS.zw = positionCS.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				float Alpha = /*ase_frag_out:Alpha;Float;0;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;1;-1;_AlphaClip*/AlphaClipThreshold/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif
				outDepth = IN.positionCS.z;
                return 0;
            }

            ENDHLSL
        }

		/*ase_pass*/
        Pass
        {
			/*ase_hide_pass*/
        	Name "DepthOnly"
            Tags{"LightMode" = "DepthOnly"}

			ZWrite On
			ColorMask 0
			AlphaToMask Off

            HLSLPROGRAM

            #pragma vertex vert
            #pragma fragment frag

			#pragma multi_compile_instancing

			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			/*ase_pragma*/

            struct VertexInput
            {
                float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				/*ase_vdata:p=p;n=n;uv0=tc0.xyzw*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 positionCS      : SV_POSITION;
                /*ase_interp(0,):sp=sp.xyzw;*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

            VertexOutput vert(VertexInput v/*ase_vert_input*/)
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				float3 vertexValue = /*ase_vert_out:Vertex Offset;Float3;2;-1;_Vertex*/ float3(0,0,0) /*end*/;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.positionOS.xyz = vertexValue;
				#else
				v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = /*ase_vert_out:Vertex Normal;Float3;3;-1;_Normal*/ v.normalOS /*end*/;

        	    o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
        	    return o;
            }

            half4 frag(VertexOutput IN, out float outDepth : SV_Depth /*ase_frag_input*/) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;

				/*ase_frag_code:IN=VertexOutput*/

				IN.positionCS.zw = positionCS.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				float Alpha = /*ase_frag_out:Alpha;Float;0;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;1;-1;_AlphaClip*/AlphaClipThreshold/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif
				outDepth = IN.positionCS.z;
                return 0;
            }
            ENDHLSL
        }

		/*ase_pass*/
		Pass
		{
			/*ase_hide_pass*/
			Name "SceneSelectionPass"
			Tags{ "LightMode" = "SceneSelectionPass" }

			ZWrite On
			ColorMask 0
			Cull Off

			HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#define SCENESELECTIONPASS 1

			#pragma multi_compile_instancing

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			/*ase_pragma*/

			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				/*ase_vdata:p=p;n=n;uv0=tc0*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS      : SV_POSITION;
				/*ase_interp(0,):sp=sp.xyzw;*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			int _ObjectId;
			int _PassValue;

			/*ase_globals*/

			/*ase_funcs*/

			VertexOutput vert(VertexInput v/*ase_vert_input*/)
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				float3 vertexValue = /*ase_vert_out:Vertex Offset;Float3;2;-1;_Vertex*/ float3(0,0,0) /*end*/;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.positionOS.xyz = vertexValue;
				#else
				v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = /*ase_vert_out:Vertex Normal;Float3;3;-1;_Normal*/ v.normalOS /*end*/;

        	    o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
        	    return o;
            }

			half4 frag(VertexOutput IN, out float outDepth : SV_Depth /*ase_frag_input*/) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);
				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;
				/*ase_frag_code:IN=GraphVertexOutput*/
				IN.positionCS.zw = positionCS.zw;
				float Alpha = /*ase_frag_out:Alpha;Float;0;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;1;-1;_AlphaClip*/AlphaClipThreshold/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				outDepth = IN.positionCS.z;
				return float4( _ObjectId, _PassValue, 1.0, 1.0 );
            }
			ENDHLSL
		}

		/*ase_pass*/
        Pass
        {
			/*ase_hide_pass*/
        	Name "Meta"
            Tags{"LightMode" = "Meta"}

			Cull Off

			HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_instancing

			#pragma shader_feature _ EDITOR_VISUALIZATION

			#define SHADERPASS SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			/*ase_pragma*/

            struct VertexInput
            {
                float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				/*ase_vdata:p=p;n=n;uv0=tc0;uv1=tc1;uv2=tc2*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 positionCS      : SV_POSITION;
				#ifdef EDITOR_VISUALIZATION
				float4 VizUV : TEXCOORD2;
				float4 LightCoord : TEXCOORD3;
				#endif
                /*ase_interp(0,):sp=sp.xyzw;uv1=tc1*/
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

            VertexOutput vert(VertexInput v/*ase_vert_input*/)
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/

				float3 vertexValue = /*ase_vert_out:Vertex Offset;Float3;4;-1;_Vertex*/ float3(0,0,0) /*end*/;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.positionOS.xyz = vertexValue;
				#else
				v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = /*ase_vert_out:Vertex Normal;Float3;5;-1;_Normal*/ v.normalOS /*end*/;

				o.positionCS = MetaVertexPosition(v.positionOS, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST);

				#ifdef EDITOR_VISUALIZATION
					float2 VizUV = 0;
					float4 LightCoord = 0;
					UnityEditorVizData(v.positionOS.xyz, v.texcoord0.xy, v.texcoord1.xy, v.texcoord2.xy, VizUV, LightCoord);
					o.VizUV = float4(VizUV, 0, 0);
					o.LightCoord = LightCoord;
				#endif

        	    return o;
            }

            half4 frag(VertexOutput IN, out float outDepth : SV_Depth /*ase_frag_input*/) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);
				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;
           		/*ase_frag_code:IN=GraphVertexOutput*/
				IN.positionCS.zw = positionCS.zw;
		        float3 Albedo = /*ase_frag_out:Albedo;Float3;0;-1;_Albedo*/float3(0.5, 0.5, 0.5)/*end*/;
				float3 Emission = /*ase_frag_out:Emission;Float3;1;-1;_Emission*/0/*end*/;
				float Alpha = /*ase_frag_out:Alpha;Float;2;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;3;-1;_AlphaClip*/0/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				outDepth = IN.positionCS.z;

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				#ifdef EDITOR_VISUALIZATION
					metaInput.VizUV = IN.VizUV.xy;
					metaInput.LightCoord = IN.LightCoord;
				#endif
				return MetaFragment(metaInput);
            }
            ENDHLSL
        }

		/*ase_pass*/
		Pass
		{
			/*ase_hide_pass*/
			Name "DepthNormals"
			Tags{"LightMode" = "DepthNormals"}

			ZWrite On
			Blend One Zero
			ZTest LEqual
			ZWrite On

			HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_instancing

			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			/*ase_pragma*/

			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				/*ase_vdata:p=p;n=n;uv0=tc0.xyzw*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};


			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				/*ase_interp(0,):sp=sp.xyzw*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

			VertexOutput vert(VertexInput v/*ase_vert_input*/)
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				float3 vertexValue = /*ase_vert_out:Vertex Offset;Float3;2;-1;_Vertex*/ float3(0,0,0) /*end*/;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.positionOS.xyz = vertexValue;
				#else
				v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = /*ase_vert_out:Vertex Normal;Float3;3;-1;_Normal*/ v.normalOS /*end*/;

				o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
				return o;
			}

			void frag(VertexOutput IN
				, out half4 outNormalWS : SV_Target0
				, out float outDepth : SV_Depth
				#ifdef _WRITE_RENDERING_LAYERS
				, out float4 outRenderingLayers : SV_Target1
				#endif
				/*ase_frag_input*/)
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;

				/*ase_frag_code:IN=VertexOutput*/

				IN.positionCS.zw = positionCS.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				float3 Normal = /*ase_frag_out:World Normal;Float3;5;-1;_FragNormal*/float3(0, 0, 1)/*end*/;
				float Alpha = /*ase_frag_out:Alpha;Float;0;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;1;-1;_AlphaClip*/AlphaClipThreshold/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif
				outDepth = IN.positionCS.z;

				#if defined(_GBUFFER_NORMALS_OCT)
					float2 octNormalWS = PackNormalOctQuadEncode(Normal);
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);
					outNormalWS = half4(packedNormalWS, 0.0);
				#else
					outNormalWS = half4(NormalizeNormalPerPixel(Normal), 0.0);
				#endif

				#ifdef _WRITE_RENDERING_LAYERS
					uint renderingLayers = GetMeshRenderingLayer();
					outRenderingLayers = float4( EncodeMeshRenderingLayer( renderingLayers ), 0, 0, 0 );
				#endif
			}
			ENDHLSL
		}

		/*ase_pass*/
		Pass
		{
			/*ase_hide_pass:SyncP*/
			Name "GBuffer"
			Tags{"LightMode" = "UniversalGBuffer"}

			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0,0
			ColorMask RGBA
			/*ase_stencil*/

			HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile_fog

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION
			#pragma multi_compile_fragment _ _SHADOWS_SOFT
			//#pragma multi_compile_fragment _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile_fragment _ _LIGHT_LAYERS
			#pragma multi_compile_fragment _ _RENDER_PASS_ENABLED
			//#pragma shader_feature_local _RECEIVE_SHADOWS_OFF
			//#pragma shader_feature_local_fragment _SPECULARHIGHLIGHTS_OFF
			//#pragma shader_feature_local_fragment _ENVIRONMENTREFLECTIONS_OFF

			//#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			//#pragma multi_compile _ SHADOWS_SHADOWMASK
			//#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			//#pragma multi_compile _ LIGHTMAP_ON
			//#pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fragment _ _GBUFFER_NORMALS_OCT
			#pragma multi_compile_fragment _ _WRITE_RENDERING_LAYERS

			#define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(LOD_FADE_CROSSFADE)
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/LODCrossFade.hlsl"
			#endif

			/*ase_pragma*/

			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 tangentOS : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				/*ase_vdata:p=p;n=n;t=t;uv0=tc0.xyzw;uv1=tc1.xyzw*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS                : SV_POSITION;
				float4 lightmapUVOrVertexSH	  : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				/*ase_interp(3,):sp=sp.xyzw;*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			/*ase_globals*/

			/*ase_funcs*/

			VertexOutput vert(VertexInput v/*ase_vert_input*/)
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				v.positionOS.xyz += /*ase_vert_out:Vertex Offset;Float3;8;-1;_Vertex*/ float3(0, 0, 0) /*end*/;

				// Vertex shader outputs defined by graph
				float3 lwWNormal = TransformObjectToWorldNormal(v.normalOS);

				VertexPositionInputs vertexInput = GetVertexPositionInputs(v.positionOS.xyz);

				// We either sample GI from lightmap or SH.
				// Lightmap UV and vertex SH coefficients use the same interpolator ("float2 lightmapUV" for lightmap or "half3 vertexSH" for SH)
				// see DECLARE_LIGHTMAP_OR_SH macro.
				// The following funcions initialize the correct variable with correct data
				OUTPUT_LIGHTMAP_UV(v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy);
				OUTPUT_SH(lwWNormal, o.lightmapUVOrVertexSH.xyz);

				half3 vertexLight = VertexLighting(vertexInput.positionWS, lwWNormal);

				o.fogFactorAndVertexLight = half4(0, vertexLight);
				o.positionCS = vertexInput.positionCS;

				//#if defined( REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR )
				//	o.shadowCoord = GetShadowCoord(vertexInput);
				//#endif
				return o;
			}

			FragmentOutput frag(VertexOutput IN, out float outDepth : SV_Depth /*ase_frag_input*/)
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;

				/*ase_frag_code:IN=VertexOutput*/

				float3 Albedo = /*ase_frag_out:Base Color;Float3;0;-1;_BaseColor*/float3( 0.5, 0.5, 0.5 )/*end*/;
				float3 Normal = /*ase_frag_out:World Normal;Float3;1;-1;_FragNormal*/float3( 0, 0, 1 )/*end*/;
				float3 Emission = /*ase_frag_out:Emission;Float3;2;-1;_Emission*/0/*end*/;
				float3 Specular = /*ase_frag_out:Specular;Float3;9;-1;_Specular*/0.5/*end*/;
				float Metallic = /*ase_frag_out:Metallic;Float;3;-1;_Metallic*/0/*end*/;
				float Smoothness = /*ase_frag_out:Smoothness;Float;4;-1;_Smoothness*/0.5/*end*/;
				float Occlusion = /*ase_frag_out:Occlusion;Float;5;-1;_Occlusion*/1/*end*/;
				float Alpha = /*ase_frag_out:Alpha;Float;6;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;7;-1;_AlphaClip*/0.5/*end*/;
				float3 BakedGI = /*ase_frag_out:Baked GI;Float3;11;-1;_BakedGI*/0/*end*/;
				float3 Transmission = /*ase_frag_out:Transmission;Float3;14;-1;_Transmission*/1/*end*/;
				float3 Translucency = /*ase_frag_out:Translucency;Float3;15;-1;_Translucency*/1/*end*/;
				
				IN.positionCS.zw = positionCS.zw;

				#ifdef LOD_FADE_CROSSFADE
					LODFadeCrossFade( IN.positionCS );
				#endif

				float3 WorldSpaceViewDirection = SafeNormalize(_WorldSpaceCameraPos.xyz - positionWS);

				InputData inputData = (InputData)0;
				inputData.positionWS = positionWS;
				inputData.normalWS = Normal;
				inputData.viewDirectionWS = normalize(WorldSpaceViewDirection);

				#if defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					inputData.shadowCoord = TransformWorldToShadowCoord(positionWS);
				#else
					inputData.shadowCoord = 0;
				#endif

				inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.positionCS);
				inputData.shadowMask = 1;

				#if defined(CUSTOM_BAKED_GI)
					half4 decodeInstructions = half4(LIGHTMAP_HDR_MULTIPLIER, LIGHTMAP_HDR_EXPONENT, 0.0h, 0.0h);
					inputData.bakedGI = UnpackLightmapRGBM(BakedGI, decodeInstructions) * EMISSIVE_RGBM_SCALE;
				#else
					inputData.bakedGI = SAMPLE_GI(IN.lightmapUVOrVertexSH.xy, IN.lightmapUVOrVertexSH.xyz, inputData.normalWS);
				#endif

				#ifdef _DBUFFER
					ApplyDecal(IN.positionCS,
						Albedo,
						Specular,
						inputData.normalWS,
						Metallic,
						Occlusion,
						Smoothness);
				#endif

				BRDFData brdfData;
				InitializeBRDFData
				(Albedo, Metallic, Specular, Smoothness, Alpha, brdfData);

				Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, inputData.shadowMask);
				half4 color;
				MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, inputData.shadowMask);
				color.rgb = GlobalIllumination(brdfData, inputData.bakedGI, Occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#if ASE_LW_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				outDepth = IN.positionCS.z;

				return BRDFDataToGbuffer(brdfData, inputData, Smoothness, Emission + color.rgb);
			}
			ENDHLSL
		}

		/*ase_pass*/
		Pass
		{
			/*ase_hide_pass*/
			Name "ScenePickingPass"
			Tags{ "LightMode" = "Picking" }

			HLSLPROGRAM

			#pragma vertex vert
			#pragma fragment frag

			#define SCENEPICKINGPASS 1

			#pragma multi_compile_instancing

			#define ATTRIBUTES_NEED_NORMAL
			#define ATTRIBUTES_NEED_TANGENT
			#define SHADERPASS SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			/*ase_pragma*/

			struct VertexInput
			{
				float4 positionOS : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				/*ase_vdata:p=p;n=n;uv0=tc0*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
					float3 positionWS : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef EDITOR_VISUALIZATION
					float4 VizUV : TEXCOORD2;
					float4 LightCoord : TEXCOORD3;
				#endif
				/*ase_interp(2,):sp=sp;wp=tc0;sc=tc1*/
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			#ifdef ASE_TRANSMISSION
				float _TransmissionShadow;
			#endif
			#ifdef ASE_TRANSLUCENCY
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			CBUFFER_END

			float4 _SelectionID;

			/*ase_globals*/

			/*ase_funcs*/

			VertexOutput vert(VertexInput v/*ase_vert_input*/)
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				/*ase_vert_code:v=VertexInput;o=VertexOutput*/
				float3 vertexValue = /*ase_vert_out:Vertex Offset;Float3;2;-1;_Vertex*/ float3(0,0,0) /*end*/;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.positionOS.xyz = vertexValue;
				#else
				v.positionOS.xyz += vertexValue;
				#endif

				v.normalOS = /*ase_vert_out:Vertex Normal;Float3;3;-1;_Normal*/ v.normalOS /*end*/;

				o.positionCS = TransformObjectToHClip(v.positionOS.xyz);
				return o;
			}

			half4 frag(VertexOutput IN /*ase_frag_input*/) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				SurfaceOutput o = (SurfaceOutput)0;
				float4 positionCS = 0;
				float3 positionWS = 0;
				/*ase_frag_code:IN=GraphVertexOutput*/
				IN.positionCS.zw = positionCS.zw;
				float Alpha = /*ase_frag_out:Alpha;Float;0;-1;_Alpha*/1/*end*/;
				float AlphaClipThreshold = /*ase_frag_out:Alpha Clip Threshold;Float;1;-1;_AlphaClip*/AlphaClipThreshold/*end*/;

				#if _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				half4 outColor = 0;
				outColor = _SelectionID;

				return outColor;
			}
			ENDHLSL
		}
		/*ase_pass_end*/
    }
    FallBack "Hidden/InternalErrorShader"
	CustomEditor "ASEMaterialInspector"
}
