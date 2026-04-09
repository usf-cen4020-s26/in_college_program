/// <reference types="vite/client" />

declare module '*.cob?raw' {
  const content: string;
  export default content;
}
