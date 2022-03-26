module.exports = {
  content: [
    "./pages/**/*.{js,ts,jsx,tsx}",
    "./components/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    colors: {
      "main": "#212239",
      "white": "#fff",
    },
    extend: {
      colors: {
        "brdr-fade": "#56566C"
      },
      backgroundImage: {
        "effect": "url('/bg_effect.webp')"
      }
    },
  },
  plugins: [],
}
