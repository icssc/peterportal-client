/** @type {import('next').NextConfig} */
const nextConfig = {
  distDir: './dist',
  async rewrites() {
    return [
      {
        source: '/api/:path*',
        destination: 'http://localhost:8080/api/:path*',
      },
    ];
  },
}
 
export default nextConfig
