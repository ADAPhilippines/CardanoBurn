import { useState } from 'react';
import type { NextPage } from 'next';
import Head from 'next/head';
import Image from 'next/image';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faGithub } from '@fortawesome/free-brands-svg-icons';
import LinearProgress from '@mui/material/LinearProgress';
import Paper from '@mui/material/Paper';
import Select from '@mui/material/Select';
import MenuItem from '@mui/material/MenuItem';
import InputBase from '@mui/material/InputBase';
import { styled } from '@mui/material/styles';
import TextField from '@mui/material/TextField';
import Button from '@mui/material/Button';

// Figure out how to integrate more closely to tailwindcss
const BootstrapInput = styled(InputBase)(({ theme }) => ({
  '& .MuiInputBase-input': {
    background: '#343352',
    borderRadius: 4,
    position: 'relative',
    border: '1px solid #56566C',
    fontSize: 16,
    padding: '10px 26px 10px 12px',
    transition: theme.transitions.create(['border-color', 'box-shadow']),
    '&:focus': {
      borderRadius: 4,
      borderColor: '#56566C',
      boxShadow: '0 0 0 0.2rem rgba(0,123,255,.25)',
    },
  },
  '& .MuiSvgIcon-root': {
    color: '#FFF'
  }
}));

const Home: NextPage = () => {
  const [age, setAge] = useState(1);
  return (
    <div className="bg-main text-white h-[100vh] bg-effect bg-cover bg-top">

      <Head>
        <title>Cardano Burn 🔥</title>
        <meta name="description" content="Burn your $ADA 🔥" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <header className="container mx-auto flex pt-6 pb-6 relative border-solid border-b-2 border-brdr-fade">
        <div className="w-1/4 h-[60px] relative">
          <Image src="/cardano_burn.svg" alt="Cardano Burn" layout="fill" />
        </div>
        <div className="absolute right-[calc(50%-237px/2)] top-[calc(50%-24px/2)] tracking-widest">🔥 Burn your $ADA for the lulz 🔥</div>
        <div className="w-full" />
        <nav className='flex items-center space-x-12'>
          <a href="#" className="font-bold tracking-widest hover:text-secondary hover:underline">ABOUT</a>
          <a href="https://github.com/ADAPhilippines/CardanoBurn" target="_blank" className="text-[32px] hover:text-secondary hover:underline">
            <FontAwesomeIcon icon={faGithub} />
          </a>
        </nav>
      </header>

      <main className="container mx-auto flex mt-12 space-x-24">
        <div className="w-1/2">
          <div className="mt-6">
            <h1 className="text-4xl font-bold">
              <span>Welcome</span>
              <br />
              <span>to </span>
              <span className="text-secondary">CardanoBurn</span>
            </h1>
            <div className="bg-secondary w-[50px] h-[1px] mt-6 mb-6" />
            <p className="text-base">
              So you've been bugging Charles Hoskin wen burn $ADA? Well here is your answer, go right ahead and burn your $ADA today! 🔥
            </p>
            <p className="text-base mt-6">
              <span>Oh now you're pretending to not know what I am talking about? Well here is a refresher: </span>
              <a className="text-secondary underline" href="https://www.youtube.com/watch?v=KaLZJs5Y_rE" target="_blank">Proof of Burn Challenge</a>
              <span> 🔗 </span>
            </p>
          </div>
          <div className="mt-12">
            <h4 className="text-2xl font-bold">
              TOTAL BURNED
            </h4>
          </div>
          <div className="mt-4">
            <h4 className="font-bold">
              <span className="text-[64px]">0 $ADA </span>
              <span className="text-[24px] text-gray">/45,000,000,000 $ADA</span>
            </h4>
            <LinearProgress className="mt-4" variant="determinate" value={50} sx={{
              height: 20,
              borderRadius: 9999,
              background: "rgba(255,255,255,0.2)",
              '& .MuiLinearProgress-bar': {
                borderRadius: 9999,
                background: 'linear-gradient(90deg, rgba(255,76,101,1) 0%, rgba(255,132,54,1) 41%, rgba(255,101,51,1) 100%)'
              }
            }} />
          </div>
        </div>
        <div className="w-1/2">
          <Paper className="backdrop-opacity-10 backdrop-invert bg-main/30 text-white" elevation={3}>
            <div className="font-bold p-6 border-solid border-b-2 border-brdr-fade">What would you like to burn? 🔥</div>
            <div className="p-6">
              <div className="flex w-full space-x-2">
                <div className="w-5/12">
                  <Select
                    value={age}
                    className="text-white w-full"
                    onChange={(e) => setAge(e.target.value as number)}
                    input={<BootstrapInput />}
                  >
                    <MenuItem value={1}>
                      <FontAwesomeIcon icon={faGithub} className="mr-2" />
                      <span>ADA</span>
                    </MenuItem>
                    <MenuItem value={2}>
                      <FontAwesomeIcon icon={faGithub} className="mr-2" />
                      <span>HOSKY</span>
                    </MenuItem>
                    <MenuItem value={3}>
                      <FontAwesomeIcon icon={faGithub} className="mr-2" />
                      <span>SUNDAE</span>
                    </MenuItem>
                  </Select>
                </div>
                <div className="w-7/12">
                  <TextField className="w-full" type="number" placeholder="How much...?" sx={{
                    border: 'none',
                    '& .MuiInputBase-input': {
                      background: '#343352',
                      position: 'relative',
                      fontSize: 16,
                      padding: '10px 26px 10px 12px',
                      color: '#FFF',
                      border: '1px solid #56566C',
                    },
                    '& .MuiOutlinedInput-notchedOutline': {
                      border: 'none'
                    }
                  }}/>
                </div>
              </div>
              <div className="pt-5 pb- w-[40%] mx-auto">
                <Button className="text-white bg-gradient w-full p-2" variant="contained">BURN MY ADA</Button>
              </div>
            </div>
          </Paper>
        </div>
      </main>
    </div>
  );
}

export default Home;
