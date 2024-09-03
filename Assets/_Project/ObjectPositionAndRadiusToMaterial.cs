using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[ExecuteAlways]
public class ObjectPositionAndRadiusToMaterial : MonoBehaviour
{
    public Material material;

    [Space]

    public string positionPropertyName = "_ObjectPosition";
    public string radiusPropertyName = "_ObjectRadius";

    [Space]

    public bool autoMaterial;

    void Start()
    {

    }

    void Update()
    {
        if (autoMaterial)
        {
            Renderer renderer = GetComponent<Renderer>();

            if (Application.isPlaying)
            {
                if (material == renderer.sharedMaterial)
                {
                    material = renderer.material;
                }
            }
            else
            {
                material = renderer.sharedMaterial;
            }
        }

        material.SetVector(positionPropertyName, transform.position);
        material.SetFloat(radiusPropertyName, transform.lossyScale.x / 2.0f);
    }

    // No radius if disabled.

    void OnDisable()
    {
        material.SetFloat(radiusPropertyName, 0.0f);
    }
}
