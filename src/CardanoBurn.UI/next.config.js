/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  env: {
    CONTRACT_ADDRESS: 'addr_test1wrw77tq9hqe7yz2vls5xvdxvvnkvx70ppl5sun4h45kzzase2k5tj'
  },
  webpack: (config) => {
    config.experiments = {
      asyncWebAssembly: true,
      layers: true,
    };
    return config;
  }
}

module.exports = nextConfig
