using UnityEngine;

namespace OpenWorldTemplate
{
    [RequireComponent(typeof(Outline), typeof(Rigidbody), typeof(BoxCollider))]
    public class WorldItem : MonoBehaviour
    {
        public BaseItem itemDataAsset;

        private void OnTriggerEnter(Collider other)
        {
            print($"{other.gameObject} triggered WorldItem: {gameObject.name}");
        }

        public void SetLayerRecursively(GameObject obj, int layer)
        {
            obj.layer = layer;
            foreach (Transform child in obj.transform)
            {
                SetLayerRecursively(child.gameObject, layer);
            }
        }

        private void OnValidate()
        {
#if UNITY_EDITOR
            if (itemDataAsset == null)
                Debug.Log($"No item data asset assigned to {this} - assign in inspector");

            Rigidbody rb = GetComponent<Rigidbody>();
            if (rb.isKinematic)
                rb.isKinematic = false;

            if (gameObject.layer != LayerMask.NameToLayer("Item"))
                gameObject.layer = LayerMask.NameToLayer("Item");
            
            foreach (Transform child in transform)
                SetLayerRecursively(child.gameObject, LayerMask.NameToLayer("Item"));

            // Outline
            if (GetComponent<Outline>().OutlineMode != Outline.Mode.OutlineVisible)
                GetComponent<Outline>().OutlineMode = Outline.Mode.OutlineVisible;

            if (GetComponent<Outline>().OutlineWidth != 4f)
                GetComponent<Outline>().OutlineWidth = 4f;

            Color outlineColor = new Color(1, 1, 1, 0.3f);
            if (GetComponent<Outline>().OutlineColor != outlineColor)
                GetComponent<Outline>().OutlineColor = outlineColor;
        }
#endif
    }
}

