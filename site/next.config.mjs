/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    unoptimized: true
  },
  async rewrites() {
    return [{
      source: '/api/:path*',
      destination: 'http://localhost:8080/api/:path*',
    }];
  },
}
 
export default nextConfig
