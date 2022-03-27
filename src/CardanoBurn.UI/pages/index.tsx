import { useEffect, useState } from 'react';
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
import CardanoLoader from '../CardanoLoader';
import { Bifrost, BifrostWalletMetadata } from '@saib/cardano-bifrost';
import SvgIcon from '@mui/material/SvgIcon';
import Dialog from '@mui/material/Dialog';
import DialogTitle from '@mui/material/DialogTitle';
import DialogContent from '@mui/material/DialogContent';
import DialogContentText from '@mui/material/DialogContentText';
import DialogActions from '@mui/material/DialogActions';
import Snackbar from '@mui/material/Snackbar';
import Alert from '@mui/material/Alert';
import { TransactionUnspentOutput } from '@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib';

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
  const [Cardano, setCardano] = useState<typeof import("@emurgo/cardano-serialization-lib-browser/cardano_serialization_lib") | null>(null);
  const [wallets, setWallets] = useState<BifrostWalletMetadata[]>([]);
  const [isBurnDialogOpen, setIsBurnDialogOpen] = useState(false);
  const [isWalletPickerOpen, setIsWalletPickerOpen] = useState(false);
  const [isErrorMessageOpen, setIsErrorMessageOpen] = useState(false);
  const [burnAmount, setBurnAmount] = useState('');

  useEffect(() => {
    const loadCardano = async () => {
      const cardano = await CardanoLoader.LoadAsync();
      setCardano(cardano);
    };

    loadCardano();
  }, []);

  const selectUtxo = (lovelaceRequired: number, utxos: TransactionUnspentOutput[]) => {
    const selectedUtxos: TransactionUnspentOutput[] = [];
    if (Cardano !== null) {
      let selectedLovelace = Cardano.BigNum.from_str("0");
      const lovelaceRequiredBigNum = Cardano.BigNum.from_str(lovelaceRequired.toString());
      utxos.forEach(utxo => {
        if (selectedLovelace.compare(lovelaceRequiredBigNum) === -1 &&
          utxo.output().amount().coin().compare(lovelaceRequiredBigNum) === 1) {
          selectedUtxos.push(utxo);
          selectedLovelace = selectedLovelace.checked_add(utxo.output().amount().coin());
        }
      });
    }
    return selectedUtxos;
  };

  const onStartBurnClicked = async () => {
    setIsBurnDialogOpen(true);
    setWallets(Bifrost.getWallets());
  };

  const onDoBurnClicked = async (walletId: string) => {
    try {
      const burnLovelace = parseInt(burnAmount) * 1_000_000;
      const isConnectionSuccess = await Bifrost.enableAsync(walletId);
      if (!isConnectionSuccess) setIsErrorMessageOpen(true);

      const rawUtxos = await Bifrost.getUtxosRawAsync();

      if (rawUtxos !== undefined && Cardano !== null) {
        const utxos = rawUtxos.map((rawUtxo) => {
          return Cardano.TransactionUnspentOutput.from_bytes(Buffer.from(rawUtxo, 'hex'));
        });

        const inputUtxos = selectUtxo(burnLovelace, utxos);
        
      }

    } catch (ex) {
      console.log(ex);
      setIsErrorMessageOpen(true);
    }
  };

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
          {/* <a href="#" className="font-bold tracking-widest hover:text-secondary hover:underline">ABOUT</a> */}
          <a href="https://github.com/ADAPhilippines/CardanoBurn" target="_blank" className="text-[32px] hover:text-secondary hover:underline">
            <FontAwesomeIcon icon={faGithub} />
          </a>
        </nav>
      </header>

      <main className="container mx-auto flex mt-12 space-x-24">
        <div className="w-1/2">
          <div className="mt-6">
            <h1 className="text-6xl font-bold">
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
        <div className="w-1/2 mt-[24px]">
          <Paper className="backdrop-opacity-10 backdrop-invert bg-main/30 text-white" elevation={3}>
            <div className="font-bold p-6 border-solid border-b-2 border-brdr-fade">What would you like to burn? 🔥</div>
            <div className="p-6 flex flex-col space-y-4">
              <div className="flex w-full space-x-2">
                <div className="w-1/2">
                  <Select
                    value={1}
                    className="text-white w-full"
                    input={<BootstrapInput />}
                  >
                    <MenuItem value={1}>
                      <div className="flex item-center">
                        <SvgIcon className="mr-2">
                          <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 375 346.51"><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><path d="M102.76,172a25.31,25.31,0,0,0,23.78,26.65c.49,0,1,0,1.46,0A25.26,25.26,0,1,0,102.76,172Z" fill="#fff" /><path d="M8.62,165.5a8.16,8.16,0,1,0,7.69,8.61A8.15,8.15,0,0,0,8.62,165.5Z" fill="#fff" /><path d="M101.16,25.43a8.16,8.16,0,1,0-11-3.62A8.18,8.18,0,0,0,101.16,25.43Z" fill="#fff" /><path d="M126.78,70.1a12.61,12.61,0,1,0-16.94-5.59A12.62,12.62,0,0,0,126.78,70.1Z" fill="#fff" /><path d="M40.58,100.82a10.39,10.39,0,1,0-3-14.38A10.39,10.39,0,0,0,40.58,100.82Z" fill="#fff" /><path d="M55.93,161a12.62,12.62,0,1,0,11.88,13.31A12.62,12.62,0,0,0,55.93,161Z" fill="#fff" /><path d="M42,245.72a10.39,10.39,0,1,0,13.95,4.6A10.37,10.37,0,0,0,42,245.72Z" fill="#fff" /><path d="M91,134.89a14.84,14.84,0,1,0-4.27-20.55A14.83,14.83,0,0,0,91,134.89Z" fill="#fff" /><path d="M246.47,69.1a12.62,12.62,0,1,0-3.63-17.47A12.61,12.61,0,0,0,246.47,69.1Z" fill="#fff" /><path d="M272.35,24.57A8.16,8.16,0,1,0,270,13.26,8.16,8.16,0,0,0,272.35,24.57Z" fill="#fff" /><path d="M248.45,147.91a25.25,25.25,0,0,0-2.87,50.42c.49,0,1,0,1.45,0a25.25,25.25,0,0,0,1.42-50.46Z" fill="#fff" /><path d="M135.08,133.14A25.12,25.12,0,0,0,157.64,147a25.25,25.25,0,0,0,22.54-36.62,25.25,25.25,0,1,0-45.1,22.73Z" fill="#fff" /><path d="M333,100.79a10.39,10.39,0,1,0-14-4.6A10.4,10.4,0,0,0,333,100.79Z" fill="#fff" /><path d="M269,108.83a14.84,14.84,0,1,0,19.94,6.58A14.86,14.86,0,0,0,269,108.83Z" fill="#fff" /><path d="M186.55,20.76a10.39,10.39,0,1,0-9.79-11A10.38,10.38,0,0,0,186.55,20.76Z" fill="#fff" /><path d="M186.43,86.13a14.84,14.84,0,1,0-14-15.66A14.84,14.84,0,0,0,186.43,86.13Z" fill="#fff" /><path d="M106,237.68a14.84,14.84,0,1,0-19.93-6.58A14.85,14.85,0,0,0,106,237.68Z" fill="#fff" /><path d="M196,107.79a25.22,25.22,0,1,0,21.14-11.41A25.28,25.28,0,0,0,196,107.79Z" fill="#fff" /><path d="M239.92,213.37a25.26,25.26,0,1,0-11.18,33.91A25.11,25.11,0,0,0,239.92,213.37Z" fill="#fff" /><path d="M284,211.62a14.84,14.84,0,1,0,4.27,20.55A14.84,14.84,0,0,0,284,211.62Z" fill="#fff" /><path d="M332.38,173.68a12.62,12.62,0,1,0-13.31,11.88A12.62,12.62,0,0,0,332.38,173.68Z" fill="#fff" /><path d="M367.3,164.71a8.16,8.16,0,1,0,7.69,8.61A8.17,8.17,0,0,0,367.3,164.71Z" fill="#fff" /><path d="M334.42,245.68a10.39,10.39,0,1,0,3,14.39A10.39,10.39,0,0,0,334.42,245.68Z" fill="#fff" /><path d="M102.65,321.94a8.16,8.16,0,1,0,2.34,11.3A8.17,8.17,0,0,0,102.65,321.94Z" fill="#fff" /><path d="M273.83,321.08a8.16,8.16,0,1,0,11,3.62A8.16,8.16,0,0,0,273.83,321.08Z" fill="#fff" /><path d="M179,238.71a25.25,25.25,0,1,0-21.14,11.41A25.1,25.1,0,0,0,179,238.71Z" fill="#fff" /><path d="M128.53,277.41a12.62,12.62,0,1,0,3.63,17.47A12.62,12.62,0,0,0,128.53,277.41Z" fill="#fff" /><path d="M187.38,325.74a10.39,10.39,0,1,0,9.78,11A10.39,10.39,0,0,0,187.38,325.74Z" fill="#fff" /><path d="M187.49,260.37a14.84,14.84,0,1,0,14,15.67A14.85,14.85,0,0,0,187.49,260.37Z" fill="#fff" /><path d="M248.21,276.4a12.62,12.62,0,1,0,17,5.59A12.62,12.62,0,0,0,248.21,276.4Z" fill="#fff" /></g></g></svg>
                        </SvgIcon>
                        <span>ADA</span>
                      </div>
                    </MenuItem>
                  </Select>
                </div>
                <div className="w-1/2">
                  <TextField className="w-full" type="number" placeholder="How much...?" value={burnAmount}
                    onChange={(e) => setBurnAmount(e.target.value)}
                    sx={{
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
                    }} />
                </div>
              </div>
              <div className="pb- w-[40%] mx-auto">
                <Button className="text-white bg-gradient w-full p-2" variant="contained" onClick={onStartBurnClicked}>BURN MY ADA</Button>
              </div>
            </div>
          </Paper>
        </div>
      </main>

      {/* Dialogs */}
      <Dialog
        open={isBurnDialogOpen}
        onClose={() => setIsBurnDialogOpen(false)}
      >
        <DialogTitle id="alert-dialog-title">
          {"Are you sure you want to burn your ADA? ⚠️"}
        </DialogTitle>
        <DialogContent>
          <DialogContentText id="alert-dialog-description">
            Your wallet is about to ask you to sign a transaction that will permanently burn your $ADA, are you sure you want to proceed?
          </DialogContentText>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setIsBurnDialogOpen(false)}>No, this is a mistake!</Button>
          <Button onClick={() => { setIsBurnDialogOpen(false); setIsWalletPickerOpen(true); }} className="text-white bg-gradient p-2 pr-4 pl-4" variant="contained" autoFocus>
            Yes, I am sure!
          </Button>
        </DialogActions>
      </Dialog>

      <Dialog
        open={isWalletPickerOpen}
        onClose={() => setIsWalletPickerOpen(false)}
      >
        <DialogTitle id="alert-dialog-title">
          {"Choose your wallet"}
        </DialogTitle>
        <DialogActions className="grid grid-cols-4 gap-1">
          {wallets.map((wallet, index) => {
            return <Button key={wallet.id} className="flex flex-col" onClick={() => onDoBurnClicked(wallet.id)}>
              <img src={wallet.icon} alt={wallet.name} width={40} height={40} />
              <span>{wallet.name}</span>
            </Button>;
          })}
        </DialogActions>
      </Dialog>

      {/* Snackbars */}
      <Snackbar anchorOrigin={{ vertical: 'bottom', horizontal: 'center' }} open={isErrorMessageOpen} autoHideDuration={6000} onClose={() => setIsErrorMessageOpen(false)}>
        <Alert onClose={() => setIsErrorMessageOpen(false)} severity="error" sx={{ width: '100%' }}>
          Burn failed, something went wrong.
        </Alert>
      </Snackbar>
    </div>
  );
}

export default Home;
