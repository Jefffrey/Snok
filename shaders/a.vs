#version 330
layout(location = 0) in vec2 ng_Position;

uniform vec2 ng_Offset;
uniform float ng_ZNear;
uniform float ng_ZFar;
uniform float ng_FrustumScale;

void main() {
    vec4 cameraPos = vec4(ng_Position.x, ng_Position.y, -1.5, 1.0) + vec4(ng_Offset.x, ng_Offset.y, 0.0, 0.0);
    vec4 clipPos;
    clipPos.xy = cameraPos.xy * ng_FrustumScale;
    clipPos.z = cameraPos.z * (ng_ZNear + ng_ZFar) / (ng_ZNear - ng_ZFar);
    clipPos.z += 2 * ng_ZNear * ng_ZFar / (ng_ZNear - ng_ZFar);
    clipPos.w = -cameraPos.z;
    gl_Position = clipPos;
}
