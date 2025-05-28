import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      '/api': 'http://localhost:8080/',
    },
  },
  css: {
    preprocessorOptions: {
      scss: {
        api: 'modern-compiler',
      },
    },
  },
  build: {
    rollupOptions: {
      output: {
        manualChunks: {
          nivo: ['@nivo/core', '@nivo/bar', '@nivo/pie'],
          miscComponentLibraries: ['react-bootstrap'],
        },
      },
    },
  },
});
