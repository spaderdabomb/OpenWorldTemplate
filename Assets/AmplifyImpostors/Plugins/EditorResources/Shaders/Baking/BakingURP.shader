// Made with Amplify Shader Editor v1.9.3.3
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Hidden/Baking URP"
{
	Properties
    {
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)

    }

    SubShader
    {
		LOD 0

		

        Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
        Cull Back
		HLSLINCLUDE
		#pragma target 3.0
		ENDHLSL

		
        Pass
        {
            Tags { "LightMode"="UniversalForward" }
            Name "Base"

            Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

            HLSLPROGRAM
            #define ASE_SRP_VERSION 140009

            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            

            // -------------------------------------
            // Lightweight Pipeline keywords
            #pragma shader_feature _SAMPLE_GI

            // -------------------------------------
            // Unity defined keywords
            #pragma multi_compile_fog

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex vert
            #pragma fragment frag

            #define ASE_NEEDS_VERT_NORMAL
            #define ASE_NEEDS_VERT_POSITION
            #pragma shader_feature_local _NORMALMAP
            #pragma shader_feature_local _ _DETAIL_MULX2 _DETAIL_SCALED
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _EMISSION
            #pragma shader_feature_local_fragment _METALLICSPECGLOSSMAP
            #pragma shader_feature_local_fragment _SPECGLOSSMAP
            #pragma shader_feature_local_fragment _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
            #pragma shader_feature_local_fragment _OCCLUSIONMAP
            #pragma shader_feature_local_fragment _SPECULAR_SETUP
            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"


            // Lighting include is needed because of GI
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"

			

            struct GraphVertexInput
            {
                float4 vertex : POSITION;
				float4 ase_normal : NORMAL;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct GraphVertexOutput
            {
                float4 position : POSITION;
				float4 ase_texcoord : TEXCOORD0;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
            };

			float2 AIBaseMapST( out float2 Offset )
			{
				#if UNITY_VERSION >= 201910
					Offset = _BaseMap_ST.zw;
					return _BaseMap_ST.xy;
				#else
					Offset = _MainTex_ST.zw;
					return _MainTex_ST.xy;
				#endif
			}
			
			float3 AIUSurfaceOutput( float2 inputUv, out float3 albedo, out float3 normal, out float3 specular, out float smoothness, out float metallic, out float occlusion, out float3 emission, out float alpha )
			{
				SurfaceData surfaceData;
				InitializeStandardLitSurfaceData(inputUv, surfaceData);
				albedo = surfaceData.albedo;
				normal = surfaceData.normalTS;
				specular = surfaceData.specular;
				smoothness = surfaceData.smoothness;
				metallic = surfaceData.metallic;
				occlusion = surfaceData.occlusion;
				emission = surfaceData.emission;
				alpha = surfaceData.alpha;
				BRDFData brdfData;
				InitializeBRDFData(surfaceData.albedo, surfaceData.metallic, surfaceData.specular, surfaceData.smoothness, surfaceData.alpha, brdfData);
				albedo = brdfData.diffuse;
				specular = brdfData.specular;
				return surfaceData.albedo;
			}
			

            GraphVertexOutput vert (GraphVertexInput v)
            {
                GraphVertexOutput o = (GraphVertexOutput)0;
                UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				float3 ase_worldTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				o.ase_texcoord1.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal.xyz);
				o.ase_texcoord2.xyz = ase_worldNormal;
				float ase_vertexTangentSign = v.ase_tangent.w * ( unity_WorldTransformParams.w >= 0.0 ? 1.0 : -1.0 );
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord3.xyz = ase_worldBitangent;
				float3 objectToViewPos = TransformWorldToView(TransformObjectToWorld(v.vertex.xyz));
				float eyeDepth = -objectToViewPos.z;
				o.ase_texcoord.z = eyeDepth;
				
				o.ase_texcoord.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord.w = 0;
				o.ase_texcoord1.w = 0;
				o.ase_texcoord2.w = 0;
				o.ase_texcoord3.w = 0;
				v.vertex.xyz +=  float3( 0, 0, 0 ) ;
                o.position = TransformObjectToHClip(v.vertex.xyz);
                return o;
            }

			void frag( GraphVertexOutput IN ,
				out half4 outGBuffer0 : SV_Target0,
				out half4 outGBuffer1 : SV_Target1,
				out half4 outGBuffer2 : SV_Target2,
				out half4 outGBuffer3 : SV_Target3,
				out half4 outGBuffer4 : SV_Target4,
				out half4 outGBuffer5 : SV_Target5,
				out half4 outGBuffer6 : SV_Target6,
				out half4 outGBuffer7 : SV_Target7,
				out float outDepth : SV_Depth
			)
            {
				UNITY_SETUP_INSTANCE_ID( IN );
				float2 Offset1_g8 = float2( 0,0 );
				float2 localAIBaseMapST1_g8 = AIBaseMapST( Offset1_g8 );
				float2 texCoord2_g8 = IN.ase_texcoord.xy * localAIBaseMapST1_g8 + Offset1_g8;
				float2 inputUv3_g8 = texCoord2_g8;
				float3 albedo3_g8 = float3( 0,0,0 );
				float3 normal3_g8 = float3( 0,0,0 );
				float3 specular3_g8 = float3( 0,0,0 );
				float smoothness3_g8 = 0.0;
				float metallic3_g8 = 0.0;
				float occlusion3_g8 = 0.0;
				float3 emission3_g8 = float3( 0,0,0 );
				float alpha3_g8 = 0.0;
				float3 localAIUSurfaceOutput3_g8 = AIUSurfaceOutput( inputUv3_g8 , albedo3_g8 , normal3_g8 , specular3_g8 , smoothness3_g8 , metallic3_g8 , occlusion3_g8 , emission3_g8 , alpha3_g8 );
				float4 appendResult240 = (float4(albedo3_g8 , 1.0));
				
				float4 appendResult256 = (float4(specular3_g8 , smoothness3_g8));
				
				float3 ase_worldTangent = IN.ase_texcoord1.xyz;
				float3 ase_worldNormal = IN.ase_texcoord2.xyz;
				float3 ase_worldBitangent = IN.ase_texcoord3.xyz;
				float3 tanToWorld0 = float3( ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x );
				float3 tanToWorld1 = float3( ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y );
				float3 tanToWorld2 = float3( ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z );
				float3 tanNormal8_g9 = normal3_g8;
				float3 worldNormal8_g9 = float3(dot(tanToWorld0,tanNormal8_g9), dot(tanToWorld1,tanNormal8_g9), dot(tanToWorld2,tanNormal8_g9));
				float eyeDepth = IN.ase_texcoord.z;
				float temp_output_4_0_g9 = ( -1.0 / UNITY_MATRIX_P[2].z );
				float temp_output_7_0_g9 = ( ( eyeDepth + temp_output_4_0_g9 ) / temp_output_4_0_g9 );
				float4 appendResult11_g9 = (float4((worldNormal8_g9*0.5 + 0.5) , temp_output_7_0_g9));
				
				float4 appendResult257 = (float4(emission3_g8 , occlusion3_g8));
				
				#ifdef _ALPHATEST_ON
				float staticSwitch244 = ( alpha3_g8 - _Cutoff );
				#else
				float staticSwitch244 = 1.0;
				#endif
				

				outGBuffer0 = appendResult240;
				outGBuffer1 = appendResult256;
				outGBuffer2 = appendResult11_g9;
				outGBuffer3 = appendResult257;
				outGBuffer4 = 0;
				outGBuffer5 = 0;
				outGBuffer6 = 0;
				outGBuffer7 = 0;
				float alpha = staticSwitch244;
				#if _AlphaClip
					clip( alpha );
				#endif
				outDepth = IN.position.z;
            }
            ENDHLSL
        }
        
	}
	
	CustomEditor "ASEMaterialInspector"
	Fallback Off
}
/*ASEBEGIN
Version=19303
Node;AmplifyShaderEditor.FunctionNode;272;2160,-192;Inherit;False;Universal Surface Output;-1;;8;4220f49336ae49347952a65dcdfa5710;0;0;8;FLOAT3;0;FLOAT3;4;FLOAT3;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT3;9;FLOAT;10
Node;AmplifyShaderEditor.RangedFloatNode;241;2269.678,295.5062;Float;False;Global;_Cutoff;_Cutoff;0;0;Fetch;True;0;0;0;False;0;False;0.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;238;2274.845,138.1202;Float;False;Constant;_Alpha1;Alpha1;2;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;243;2516.462,261.5877;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;257;2622.835,21.34941;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.FunctionNode;187;2599.477,-190.5401;Inherit;False;Pack Normal Depth;-1;;9;8e386dbec347c9f44befea8ff816d188;0;1;12;FLOAT3;0,0,0;False;3;FLOAT4;0;FLOAT3;14;FLOAT;15
Node;AmplifyShaderEditor.DynamicAppendNode;256;2618.15,-71.96327;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.DynamicAppendNode;240;2629.412,-303.1327;Inherit;False;FLOAT4;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT4;0
Node;AmplifyShaderEditor.StaticSwitch;244;2724.947,165.0664;Float;False;Property;_Keyword4;Keyword 4;7;0;Fetch;False;0;0;0;False;0;False;0;0;0;False;_ALPHATEST_ON;Toggle;2;Key0;Key1;Fetch;False;True;All;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;271;3048.978,-256.7144;Float;False;True;-1;2;ASEMaterialInspector;0;26;Hidden/Baking URP;6ee191abcace33c46a5dd52068b074e0;True;Base;0;0;Base;10;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;False;0;False;True;1;1;False;;0;False;;0;1;False;;0;False;;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;;False;False;False;False;False;False;False;True;False;255;False;;255;False;;255;False;;7;False;;1;False;;1;False;;1;False;;7;False;;1;False;;1;False;;1;False;;False;True;1;False;;True;3;False;;True;True;0;False;;0;False;;True;1;LightMode=UniversalForward;False;True;12;all;0;;0;0;Standard;1;Receive Shadows;1;0;0;1;True;False;;False;0
WireConnection;243;0;272;10
WireConnection;243;1;241;0
WireConnection;257;0;272;9
WireConnection;257;3;272;8
WireConnection;187;12;272;4
WireConnection;256;0;272;5
WireConnection;256;3;272;6
WireConnection;240;0;272;0
WireConnection;240;3;238;0
WireConnection;244;1;238;0
WireConnection;244;0;243;0
WireConnection;271;0;240;0
WireConnection;271;1;256;0
WireConnection;271;2;187;0
WireConnection;271;3;257;0
WireConnection;271;8;244;0
ASEEND*/
//CHKSM=11AE5416A373D152E8E821813D741AA5C1E6E6C6