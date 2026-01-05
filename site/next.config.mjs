/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    unoptimized: true
  },
  basePath: '/planner',
  async rewrites() {
    return [{
      source: '/api/:path*',
      destination: 'http://localhost:8080/api/:path*',
    }];
  },

  async redirects() {
    return [{
      source: '/roadmap',
      destination: '/',
      permanent: true,
    }];
  }
};
 
export default nextConfig
