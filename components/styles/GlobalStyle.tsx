import { createGlobalStyle } from 'styled-components';

const GlobalStyle = createGlobalStyle`
  body {
    background-color: #1c2025;
  }
  
  @media only screen and (max-width: 600px) {
    .MuiBreadcrumbs-li {
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 200px;
    }
  }
`;

export default GlobalStyle;
