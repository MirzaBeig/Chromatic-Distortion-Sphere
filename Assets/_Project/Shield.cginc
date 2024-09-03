#ifndef SHIELD_CGINC
#define SHIELD_CGINC

float3 TwistXZ(float3 position, float twist)
{
    float angle = position.y * twist;
    
    float cosAngle = cos(angle);
    float sinAngle = sin(angle);
    
    float3 twistedPosition;
    
    twistedPosition.x = (position.x * cosAngle) - (position.z * sinAngle);
    twistedPosition.y = position.y;
    twistedPosition.z = (position.x * sinAngle) + (position.z * cosAngle);

    return twistedPosition;
}

float3 RGBSplit(sampler2D tex, float2 uv, float rgbSplit, uint rgbSplitSteps)
{
    float3 rgbShift = 0.0;
    
    for (uint i = 0; i < rgbSplitSteps; i++)
    {
        float progress = (i + 1.0) / rgbSplitSteps;
        
        float offset = rgbSplit * progress;
        
        float2 offset_r = float2(-offset, -offset);
        float2 offset_g = float2(+0.0, +offset);
        float2 offset_b = float2(+offset, -offset);
                
        rgbShift.r += tex2D(tex, uv - offset_r).r;
        rgbShift.g += tex2D(tex, uv - offset_g).g;
        rgbShift.b += tex2D(tex, uv - offset_b).b;
    }
    
    rgbShift /= rgbSplitSteps;
    
    return rgbShift;
}

//float Hash3D(float3 p)
//{
//    return frac(sin(dot(p, float3(12.9898, 78.233, 45.164))) * 43758.5453);
//}
float3 Hash3D(float3 p)
{
    p = float3(dot(p, float3(127.1, 311.7, 74.7)),
               dot(p, float3(269.5, 183.3, 246.1)),
			   dot(p, float3(113.5, 271.9, 124.6)));

    return frac(sin(p) * 43758.5453123);
}
float3 Hash4D(float3 uv, float angle)
{
    float3x3 m = float3x3(
    
    15.27, 47.63, 99.41,
    89.98, 54.23, 65.34,
    75.56, 35.24, 86.75);
    
    uv = frac(sin(mul(uv, m)) * 46839.32);
    uv = float3((sin(uv.y * angle) * 0.5) + 0.5, (cos(uv.x * angle) * 0.5) + 0.5, (sin(uv.z * angle) * 0.5) + 0.5);
    
    return uv;
}

float Voronoi4D(float3 position, float angle)
{
    float3 baseCell = floor(position);
    float3 fractionalPart = frac(position);
    
    float minimumDistance = 1e9;

    for (int zOffset = -1; zOffset <= 1; zOffset++)
    {
        for (int yOffset = -1; yOffset <= 1; yOffset++)
        {
            for (int xOffset = -1; xOffset <= 1; xOffset++)
            {
                float3 neighboringCellOffset = float3(xOffset, yOffset, zOffset);
                float3 randomOffset = Hash4D(baseCell + neighboringCellOffset, angle);
                
                //float3 distanceVector = (neighboringCellOffset - fractionalPart) + randomOffset;
                //float distance = length(distanceVector);
                
                // This is noticeably faster than above once you introduce procedural curl (which causes noise samples to spike, and the performance benefit comes through).
                
                float3 cellPosition = baseCell + neighboringCellOffset;
                float3 pointPosition = cellPosition + randomOffset - position;
                
                float distance = dot(pointPosition, pointPosition);
                
                minimumDistance = min(minimumDistance, distance);
            }
        }
    }
    
    return sqrt(minimumDistance);
}

float Grid3D(float3 position, float thickness)
{
    float3 lineXYZ = abs(fmod(position, 1.0)) < thickness;
    return max(max(lineXYZ.x, lineXYZ.y), lineXYZ.z);
}

void HollowSphereLight_float(float3 position, float radius, float thickness, float falloff, out float output)
{
    float sdf = abs(length(position) - radius) - thickness;
    output = 1.0 / (pow(sdf, falloff) + 1.0);
}

#endif