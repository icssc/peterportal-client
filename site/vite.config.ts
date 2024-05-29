import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  base: './',
  server: {
    proxy: {
      '/api': 'http://localhost:8080/',
    },
  },
  build: {
    rollupOptions: {
      output: {
        manualChunks: {
          dnd: ['react-beautiful-dnd'],
          fuzzySearch: ['websoc-fuzzy-search'],
          nivo: ['@nivo/core', '@nivo/bar', '@nivo/pie'],
          miscComponentLibraries: ['semantic-ui-react', 'react-bootstrap', 'react-bootstrap-icons'],
        },
      },
    },
  },
});
