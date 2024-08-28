//------------------------------------------------------------------------------
// <auto-generated>
//     This code was auto-generated by com.unity.inputsystem:InputActionCodeGenerator
//     version 1.7.0
//     from Assets/GinjaGaming/FinalCharacterController/Input/PlayerControls.inputactions
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine.InputSystem;
using UnityEngine.InputSystem.Utilities;

namespace GinjaGaming.FinalCharacterController
{
    public partial class @PlayerControls: IInputActionCollection2, IDisposable
    {
        public InputActionAsset asset { get; }
        public @PlayerControls()
        {
            asset = InputActionAsset.FromJson(@"{
    ""name"": ""PlayerControls"",
    ""maps"": [
        {
            ""name"": ""PlayerLocomotionMap"",
            ""id"": ""f59a865f-2b9f-4203-8d22-9b74a4c17d93"",
            ""actions"": [
                {
                    ""name"": ""Movement"",
                    ""type"": ""Value"",
                    ""id"": ""81b3524c-4292-4172-8a6a-a0e57f711d92"",
                    ""expectedControlType"": ""Vector2"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": true
                },
                {
                    ""name"": ""Look"",
                    ""type"": ""Value"",
                    ""id"": ""54b17b1d-e68e-4e1e-9e7c-e3fce7b5048c"",
                    ""expectedControlType"": ""Vector2"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": true
                },
                {
                    ""name"": ""ToggleSprint"",
                    ""type"": ""Button"",
                    ""id"": ""2d62136a-c146-4b49-b506-7d903b95e54a"",
                    ""expectedControlType"": ""Button"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": false
                },
                {
                    ""name"": ""Jump"",
                    ""type"": ""Button"",
                    ""id"": ""d5776ba5-3d05-42a1-87be-079b40e07be1"",
                    ""expectedControlType"": ""Button"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": false
                },
                {
                    ""name"": ""ToggleWalk"",
                    ""type"": ""Button"",
                    ""id"": ""9860aac1-4769-40e0-a3b0-cdf19f96bb78"",
                    ""expectedControlType"": ""Button"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": false
                }
            ],
            ""bindings"": [
                {
                    ""name"": ""WASD"",
                    ""id"": ""6a9f5897-9f4e-43e8-b144-9a3e3b78171a"",
                    ""path"": ""2DVector"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": true,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": ""up"",
                    ""id"": ""52b170d0-eb32-40e3-844c-bd531d7b54a7"",
                    ""path"": ""<Keyboard>/w"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""down"",
                    ""id"": ""7d7f3ffe-5573-41f4-861c-c7cc10106824"",
                    ""path"": ""<Keyboard>/s"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""left"",
                    ""id"": ""ce12a4b3-a842-4d1a-bb78-36ba6dcf6ee2"",
                    ""path"": ""<Keyboard>/a"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""right"",
                    ""id"": ""ca58bd5f-c8a9-4f40-9c37-18eaa16ca99e"",
                    ""path"": ""<Keyboard>/d"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""Left Stick"",
                    ""id"": ""9b7c3383-50f8-456a-818b-f47c538a562d"",
                    ""path"": ""2DVector"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": true,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": ""up"",
                    ""id"": ""0d8fe1d9-73af-4278-98d0-47a56a910e25"",
                    ""path"": ""<Gamepad>/leftStick/up"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""down"",
                    ""id"": ""a6058edd-c65d-40bc-bfaf-bdb7f9594736"",
                    ""path"": ""<Gamepad>/leftStick/down"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""left"",
                    ""id"": ""a82d5f42-67e7-42e1-9a0e-381e66fe7c47"",
                    ""path"": ""<Gamepad>/leftStick/left"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": ""right"",
                    ""id"": ""3a753140-66d8-4c6c-ac0b-63fadfa2ef1a"",
                    ""path"": ""<Gamepad>/leftStick/right"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Movement"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": true
                },
                {
                    ""name"": """",
                    ""id"": ""3df7d211-c245-424c-a1f6-114ddf272b10"",
                    ""path"": ""<Mouse>/delta"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Look"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": """",
                    ""id"": ""62632ce9-27da-473b-9c43-5784ec4992a5"",
                    ""path"": ""<Keyboard>/leftShift"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""ToggleSprint"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": """",
                    ""id"": ""9c11f6d6-bbfc-4c6d-9d87-1b308d464dbe"",
                    ""path"": ""<Keyboard>/space"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Jump"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": """",
                    ""id"": ""16f772a4-2cae-4c33-92df-35119815c8b2"",
                    ""path"": ""<Keyboard>/ctrl"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""ToggleWalk"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                }
            ]
        },
        {
            ""name"": ""ThirdPersonMap"",
            ""id"": ""9265275f-811a-4e98-b05f-0fb619bfb4bc"",
            ""actions"": [
                {
                    ""name"": ""ScrollCamera"",
                    ""type"": ""Value"",
                    ""id"": ""a138cf60-637d-492c-b399-7039e94b7550"",
                    ""expectedControlType"": ""Vector2"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": true
                }
            ],
            ""bindings"": [
                {
                    ""name"": """",
                    ""id"": ""f27d8ef5-6fe8-4349-a419-7f4e32607aea"",
                    ""path"": ""<Mouse>/scroll"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""ScrollCamera"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                }
            ]
        },
        {
            ""name"": ""PlayerActionsMap"",
            ""id"": ""e757dc11-d0c6-4741-abac-4eed7052bd1f"",
            ""actions"": [
                {
                    ""name"": ""Gathering"",
                    ""type"": ""Button"",
                    ""id"": ""d9ba9315-bc1d-4791-ad3e-9601169f85de"",
                    ""expectedControlType"": ""Button"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": false
                },
                {
                    ""name"": ""Attacking"",
                    ""type"": ""Button"",
                    ""id"": ""42c2e099-ae12-4034-aaba-bb528b4d1e46"",
                    ""expectedControlType"": ""Button"",
                    ""processors"": """",
                    ""interactions"": """",
                    ""initialStateCheck"": false
                }
            ],
            ""bindings"": [
                {
                    ""name"": """",
                    ""id"": ""015ef00d-d639-408a-aa1b-0ce979b7da0a"",
                    ""path"": ""<Keyboard>/e"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Gathering"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                },
                {
                    ""name"": """",
                    ""id"": ""e6f26d82-02fe-4ade-9b05-d4f8c0a271d2"",
                    ""path"": ""<Mouse>/leftButton"",
                    ""interactions"": """",
                    ""processors"": """",
                    ""groups"": """",
                    ""action"": ""Attacking"",
                    ""isComposite"": false,
                    ""isPartOfComposite"": false
                }
            ]
        }
    ],
    ""controlSchemes"": []
}");
            // PlayerLocomotionMap
            m_PlayerLocomotionMap = asset.FindActionMap("PlayerLocomotionMap", throwIfNotFound: true);
            m_PlayerLocomotionMap_Movement = m_PlayerLocomotionMap.FindAction("Movement", throwIfNotFound: true);
            m_PlayerLocomotionMap_Look = m_PlayerLocomotionMap.FindAction("Look", throwIfNotFound: true);
            m_PlayerLocomotionMap_ToggleSprint = m_PlayerLocomotionMap.FindAction("ToggleSprint", throwIfNotFound: true);
            m_PlayerLocomotionMap_Jump = m_PlayerLocomotionMap.FindAction("Jump", throwIfNotFound: true);
            m_PlayerLocomotionMap_ToggleWalk = m_PlayerLocomotionMap.FindAction("ToggleWalk", throwIfNotFound: true);
            // ThirdPersonMap
            m_ThirdPersonMap = asset.FindActionMap("ThirdPersonMap", throwIfNotFound: true);
            m_ThirdPersonMap_ScrollCamera = m_ThirdPersonMap.FindAction("ScrollCamera", throwIfNotFound: true);
            // PlayerActionsMap
            m_PlayerActionsMap = asset.FindActionMap("PlayerActionsMap", throwIfNotFound: true);
            m_PlayerActionsMap_Gathering = m_PlayerActionsMap.FindAction("Gathering", throwIfNotFound: true);
            m_PlayerActionsMap_Attacking = m_PlayerActionsMap.FindAction("Attacking", throwIfNotFound: true);
        }

        public void Dispose()
        {
            UnityEngine.Object.Destroy(asset);
        }

        public InputBinding? bindingMask
        {
            get => asset.bindingMask;
            set => asset.bindingMask = value;
        }

        public ReadOnlyArray<InputDevice>? devices
        {
            get => asset.devices;
            set => asset.devices = value;
        }

        public ReadOnlyArray<InputControlScheme> controlSchemes => asset.controlSchemes;

        public bool Contains(InputAction action)
        {
            return asset.Contains(action);
        }

        public IEnumerator<InputAction> GetEnumerator()
        {
            return asset.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Enable()
        {
            asset.Enable();
        }

        public void Disable()
        {
            asset.Disable();
        }

        public IEnumerable<InputBinding> bindings => asset.bindings;

        public InputAction FindAction(string actionNameOrId, bool throwIfNotFound = false)
        {
            return asset.FindAction(actionNameOrId, throwIfNotFound);
        }

        public int FindBinding(InputBinding bindingMask, out InputAction action)
        {
            return asset.FindBinding(bindingMask, out action);
        }

        // PlayerLocomotionMap
        private readonly InputActionMap m_PlayerLocomotionMap;
        private List<IPlayerLocomotionMapActions> m_PlayerLocomotionMapActionsCallbackInterfaces = new List<IPlayerLocomotionMapActions>();
        private readonly InputAction m_PlayerLocomotionMap_Movement;
        private readonly InputAction m_PlayerLocomotionMap_Look;
        private readonly InputAction m_PlayerLocomotionMap_ToggleSprint;
        private readonly InputAction m_PlayerLocomotionMap_Jump;
        private readonly InputAction m_PlayerLocomotionMap_ToggleWalk;
        public struct PlayerLocomotionMapActions
        {
            private @PlayerControls m_Wrapper;
            public PlayerLocomotionMapActions(@PlayerControls wrapper) { m_Wrapper = wrapper; }
            public InputAction @Movement => m_Wrapper.m_PlayerLocomotionMap_Movement;
            public InputAction @Look => m_Wrapper.m_PlayerLocomotionMap_Look;
            public InputAction @ToggleSprint => m_Wrapper.m_PlayerLocomotionMap_ToggleSprint;
            public InputAction @Jump => m_Wrapper.m_PlayerLocomotionMap_Jump;
            public InputAction @ToggleWalk => m_Wrapper.m_PlayerLocomotionMap_ToggleWalk;
            public InputActionMap Get() { return m_Wrapper.m_PlayerLocomotionMap; }
            public void Enable() { Get().Enable(); }
            public void Disable() { Get().Disable(); }
            public bool enabled => Get().enabled;
            public static implicit operator InputActionMap(PlayerLocomotionMapActions set) { return set.Get(); }
            public void AddCallbacks(IPlayerLocomotionMapActions instance)
            {
                if (instance == null || m_Wrapper.m_PlayerLocomotionMapActionsCallbackInterfaces.Contains(instance)) return;
                m_Wrapper.m_PlayerLocomotionMapActionsCallbackInterfaces.Add(instance);
                @Movement.started += instance.OnMovement;
                @Movement.performed += instance.OnMovement;
                @Movement.canceled += instance.OnMovement;
                @Look.started += instance.OnLook;
                @Look.performed += instance.OnLook;
                @Look.canceled += instance.OnLook;
                @ToggleSprint.started += instance.OnToggleSprint;
                @ToggleSprint.performed += instance.OnToggleSprint;
                @ToggleSprint.canceled += instance.OnToggleSprint;
                @Jump.started += instance.OnJump;
                @Jump.performed += instance.OnJump;
                @Jump.canceled += instance.OnJump;
                @ToggleWalk.started += instance.OnToggleWalk;
                @ToggleWalk.performed += instance.OnToggleWalk;
                @ToggleWalk.canceled += instance.OnToggleWalk;
            }

            private void UnregisterCallbacks(IPlayerLocomotionMapActions instance)
            {
                @Movement.started -= instance.OnMovement;
                @Movement.performed -= instance.OnMovement;
                @Movement.canceled -= instance.OnMovement;
                @Look.started -= instance.OnLook;
                @Look.performed -= instance.OnLook;
                @Look.canceled -= instance.OnLook;
                @ToggleSprint.started -= instance.OnToggleSprint;
                @ToggleSprint.performed -= instance.OnToggleSprint;
                @ToggleSprint.canceled -= instance.OnToggleSprint;
                @Jump.started -= instance.OnJump;
                @Jump.performed -= instance.OnJump;
                @Jump.canceled -= instance.OnJump;
                @ToggleWalk.started -= instance.OnToggleWalk;
                @ToggleWalk.performed -= instance.OnToggleWalk;
                @ToggleWalk.canceled -= instance.OnToggleWalk;
            }

            public void RemoveCallbacks(IPlayerLocomotionMapActions instance)
            {
                if (m_Wrapper.m_PlayerLocomotionMapActionsCallbackInterfaces.Remove(instance))
                    UnregisterCallbacks(instance);
            }

            public void SetCallbacks(IPlayerLocomotionMapActions instance)
            {
                foreach (var item in m_Wrapper.m_PlayerLocomotionMapActionsCallbackInterfaces)
                    UnregisterCallbacks(item);
                m_Wrapper.m_PlayerLocomotionMapActionsCallbackInterfaces.Clear();
                AddCallbacks(instance);
            }
        }
        public PlayerLocomotionMapActions @PlayerLocomotionMap => new PlayerLocomotionMapActions(this);

        // ThirdPersonMap
        private readonly InputActionMap m_ThirdPersonMap;
        private List<IThirdPersonMapActions> m_ThirdPersonMapActionsCallbackInterfaces = new List<IThirdPersonMapActions>();
        private readonly InputAction m_ThirdPersonMap_ScrollCamera;
        public struct ThirdPersonMapActions
        {
            private @PlayerControls m_Wrapper;
            public ThirdPersonMapActions(@PlayerControls wrapper) { m_Wrapper = wrapper; }
            public InputAction @ScrollCamera => m_Wrapper.m_ThirdPersonMap_ScrollCamera;
            public InputActionMap Get() { return m_Wrapper.m_ThirdPersonMap; }
            public void Enable() { Get().Enable(); }
            public void Disable() { Get().Disable(); }
            public bool enabled => Get().enabled;
            public static implicit operator InputActionMap(ThirdPersonMapActions set) { return set.Get(); }
            public void AddCallbacks(IThirdPersonMapActions instance)
            {
                if (instance == null || m_Wrapper.m_ThirdPersonMapActionsCallbackInterfaces.Contains(instance)) return;
                m_Wrapper.m_ThirdPersonMapActionsCallbackInterfaces.Add(instance);
                @ScrollCamera.started += instance.OnScrollCamera;
                @ScrollCamera.performed += instance.OnScrollCamera;
                @ScrollCamera.canceled += instance.OnScrollCamera;
            }

            private void UnregisterCallbacks(IThirdPersonMapActions instance)
            {
                @ScrollCamera.started -= instance.OnScrollCamera;
                @ScrollCamera.performed -= instance.OnScrollCamera;
                @ScrollCamera.canceled -= instance.OnScrollCamera;
            }

            public void RemoveCallbacks(IThirdPersonMapActions instance)
            {
                if (m_Wrapper.m_ThirdPersonMapActionsCallbackInterfaces.Remove(instance))
                    UnregisterCallbacks(instance);
            }

            public void SetCallbacks(IThirdPersonMapActions instance)
            {
                foreach (var item in m_Wrapper.m_ThirdPersonMapActionsCallbackInterfaces)
                    UnregisterCallbacks(item);
                m_Wrapper.m_ThirdPersonMapActionsCallbackInterfaces.Clear();
                AddCallbacks(instance);
            }
        }
        public ThirdPersonMapActions @ThirdPersonMap => new ThirdPersonMapActions(this);

        // PlayerActionsMap
        private readonly InputActionMap m_PlayerActionsMap;
        private List<IPlayerActionsMapActions> m_PlayerActionsMapActionsCallbackInterfaces = new List<IPlayerActionsMapActions>();
        private readonly InputAction m_PlayerActionsMap_Gathering;
        private readonly InputAction m_PlayerActionsMap_Attacking;
        public struct PlayerActionsMapActions
        {
            private @PlayerControls m_Wrapper;
            public PlayerActionsMapActions(@PlayerControls wrapper) { m_Wrapper = wrapper; }
            public InputAction @Gathering => m_Wrapper.m_PlayerActionsMap_Gathering;
            public InputAction @Attacking => m_Wrapper.m_PlayerActionsMap_Attacking;
            public InputActionMap Get() { return m_Wrapper.m_PlayerActionsMap; }
            public void Enable() { Get().Enable(); }
            public void Disable() { Get().Disable(); }
            public bool enabled => Get().enabled;
            public static implicit operator InputActionMap(PlayerActionsMapActions set) { return set.Get(); }
            public void AddCallbacks(IPlayerActionsMapActions instance)
            {
                if (instance == null || m_Wrapper.m_PlayerActionsMapActionsCallbackInterfaces.Contains(instance)) return;
                m_Wrapper.m_PlayerActionsMapActionsCallbackInterfaces.Add(instance);
                @Gathering.started += instance.OnGathering;
                @Gathering.performed += instance.OnGathering;
                @Gathering.canceled += instance.OnGathering;
                @Attacking.started += instance.OnAttacking;
                @Attacking.performed += instance.OnAttacking;
                @Attacking.canceled += instance.OnAttacking;
            }

            private void UnregisterCallbacks(IPlayerActionsMapActions instance)
            {
                @Gathering.started -= instance.OnGathering;
                @Gathering.performed -= instance.OnGathering;
                @Gathering.canceled -= instance.OnGathering;
                @Attacking.started -= instance.OnAttacking;
                @Attacking.performed -= instance.OnAttacking;
                @Attacking.canceled -= instance.OnAttacking;
            }

            public void RemoveCallbacks(IPlayerActionsMapActions instance)
            {
                if (m_Wrapper.m_PlayerActionsMapActionsCallbackInterfaces.Remove(instance))
                    UnregisterCallbacks(instance);
            }

            public void SetCallbacks(IPlayerActionsMapActions instance)
            {
                foreach (var item in m_Wrapper.m_PlayerActionsMapActionsCallbackInterfaces)
                    UnregisterCallbacks(item);
                m_Wrapper.m_PlayerActionsMapActionsCallbackInterfaces.Clear();
                AddCallbacks(instance);
            }
        }
        public PlayerActionsMapActions @PlayerActionsMap => new PlayerActionsMapActions(this);
        public interface IPlayerLocomotionMapActions
        {
            void OnMovement(InputAction.CallbackContext context);
            void OnLook(InputAction.CallbackContext context);
            void OnToggleSprint(InputAction.CallbackContext context);
            void OnJump(InputAction.CallbackContext context);
            void OnToggleWalk(InputAction.CallbackContext context);
        }
        public interface IThirdPersonMapActions
        {
            void OnScrollCamera(InputAction.CallbackContext context);
        }
        public interface IPlayerActionsMapActions
        {
            void OnGathering(InputAction.CallbackContext context);
            void OnAttacking(InputAction.CallbackContext context);
        }
    }
}
