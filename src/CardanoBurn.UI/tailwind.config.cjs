module.exports = {
  content: [
    "./src/**/*.{js,jsx,ts,tsx}",
  ],
  theme: {
    colors: {
      "main": "#212239",
      "white": "#fff",
      "secondary": "#FF4C65",
      "gray": "#A6A7B0"
    },
    extend: {
      colors: {
        "brdr-fade": "#56566C",
      },
      backgroundImage: {
        "effect": "url('/bg_effect.webp')",
        "gradient": "linear-gradient(90deg, rgba(255,76,101,1) 0%, rgba(255,132,54,1) 41%, rgba(255,101,51,1) 100%)"
      }
    },
  },
  plugins: [],
}
